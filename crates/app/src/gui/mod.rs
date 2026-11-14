//! The desktop app.  Layout: a session sidebar on the left, a header with
//! tabs, a scrolling result stream, and a composer at the bottom.

mod icons;
mod mathview;
mod plotview;
pub mod theme;

use eframe::egui::{self, pos2, vec2, Align, Align2, Color32, CornerRadius, FontId, Key, Layout, Margin, Rect, RichText, Sense, Stroke, StrokeKind, Ui};
use noesis::engine::{content_text, Content, Options, Pod, QueryResult};
use serde::{Deserialize, Serialize};
use std::sync::mpsc::{channel, Receiver, Sender};
use std::time::Instant;
use theme::Palette;

const CONTENT_WIDTH: f32 = 860.0;

pub fn run() -> eframe::Result<()> {
    let options = eframe::NativeOptions {
        viewport: egui::ViewportBuilder::default().with_title("Noesis").with_inner_size([1320.0, 860.0]).with_min_inner_size([760.0, 520.0]),
        ..Default::default()
    };
    eframe::run_native("Noesis", options, Box::new(|cc| Ok(Box::new(App::new(cc)))))
}

#[derive(Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Debug)]
enum Mode {
    Exact,
    Approximate,
    Steps,
}

impl Mode {
    fn label(self) -> &'static str {
        match self {
            Mode::Exact => "Exact mode",
            Mode::Approximate => "Approximate mode",
            Mode::Steps => "Step-by-step mode",
        }
    }
    fn short(self) -> &'static str {
        match self {
            Mode::Exact => "Exact",
            Mode::Approximate => "Approximate",
            Mode::Steps => "Step-by-step",
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Debug)]
enum ThemeChoice {
    Dark,
    Light,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Tab {
    Answer,
    Trace,
}

#[derive(Clone, Serialize, Deserialize)]
struct Session {
    id: u64,
    query: String,
    created: i64,
    #[serde(skip)]
    result: Option<QueryResult>,
    #[serde(skip)]
    running: bool,
    #[serde(skip)]
    mode_used: Option<Mode>,
}

#[derive(Serialize, Deserialize)]
struct Saved {
    sessions: Vec<Session>,
    theme: ThemeChoice,
    mode: Mode,
    device: String,
    digits: usize,
}

pub struct App {
    sessions: Vec<Session>,
    current: Option<usize>,
    input: String,
    tab: Tab,
    mode: Mode,
    theme: ThemeChoice,
    device: String,
    digits: usize,
    sidebar_open: bool,
    search: String,
    search_open: bool,
    settings_open: bool,
    tx: Sender<(u64, QueryResult)>,
    rx: Receiver<(u64, QueryResult)>,
    plots: plotview::States,
    toast: Option<(String, Instant)>,
    next_id: u64,
    focus_input: bool,
    bench: Option<Vec<(String, f64)>>,
}

const EXAMPLES: &[(&str, &[&str])] = &[
    ("Calculus", &["integrate x^2 sin(x) dx", "derivative of x^x", "limit (sin x - x)/x^3 as x -> 0", "sum 1/k^2, k=1 to inf"]),
    ("Algebra", &["solve x^3 - 6x^2 + 11x - 6 = 0", "factor x^4 + 4", "{x + y = 3, 2x - y = 0}", "x^2 + y^2 = 25"]),
    ("Plots", &["plot sin(x)/x", "plot x^2 - y^2", "polar plot r = 1 + cos(theta)", "mandelbrot set"]),
    ("Numbers", &["100!", "pi", "is 2^61 - 1 prime", "355/113"]),
    ("Linear algebra", &["{{2, 1}, {1, 3}}", "{{a, b}, {c, d}}"]),
    ("Science & data", &["molar mass of caffeine", "10 miles in km", "speed of light", "iron"]),
    ("World & time", &["distance from Paris to Tokyo", "time in Sydney", "days until christmas", "france"]),
    ("Statistics & money", &["mean of 3, 7, 7, 19, 24", "normal distribution mean 100 sd 15 P(X > 130)", "mortgage $300000 at 6.5% for 30 years"]),
];

fn config_path() -> Option<std::path::PathBuf> {
    let base = if cfg!(windows) {
        std::env::var_os("APPDATA").map(std::path::PathBuf::from)
    } else {
        std::env::var_os("XDG_CONFIG_HOME").map(std::path::PathBuf::from).or_else(|| std::env::var_os("HOME").map(|h| std::path::PathBuf::from(h).join(".config")))
    }?;
    Some(base.join("noesis").join("state.json"))
}

fn export_dir() -> std::path::PathBuf {
    let home = std::env::var_os(if cfg!(windows) { "USERPROFILE" } else { "HOME" }).map(std::path::PathBuf::from).unwrap_or_else(std::env::temp_dir);
    let docs = home.join("Documents");
    let base = if docs.is_dir() { docs } else { home };
    base.join("Noesis")
}

impl App {
    fn new(cc: &eframe::CreationContext<'_>) -> App {
        theme::install_fonts(&cc.egui_ctx);
        let (tx, rx) = channel();
        let mut app = App {
            sessions: vec![],
            current: None,
            input: String::new(),
            tab: Tab::Answer,
            mode: Mode::Exact,
            theme: ThemeChoice::Dark,
            device: "auto".into(),
            digits: 6,
            sidebar_open: true,
            search: String::new(),
            search_open: false,
            settings_open: false,
            tx,
            rx,
            plots: Default::default(),
            toast: None,
            next_id: 1,
            focus_input: true,
            bench: None,
        };
        if let Some(saved) = config_path().and_then(|p| std::fs::read_to_string(p).ok()).and_then(|s| serde_json::from_str::<Saved>(&s).ok()) {
            app.sessions = saved.sessions;
            app.theme = saved.theme;
            app.mode = saved.mode;
            app.device = saved.device;
            app.digits = saved.digits.clamp(3, 30);
            app.next_id = app.sessions.iter().map(|s| s.id).max().unwrap_or(0) + 1;
        }
        noesis::compute::set_preference(&app.device);
        if let Ok(t) = std::env::var("NOESIS_THEME") {
            app.theme = if t == "light" { ThemeChoice::Light } else { ThemeChoice::Dark };
        }
        theme::apply(&cc.egui_ctx, &app.palette());
        // NOESIS_QUERY: run a query at start-up (demos and screenshots)
        if let Ok(q) = std::env::var("NOESIS_QUERY") {
            app.submit(&cc.egui_ctx, q);
        }
        // warm up device detection off the UI thread
        std::thread::spawn(noesis::compute::info);
        app
    }

    fn palette(&self) -> Palette {
        match self.theme {
            ThemeChoice::Dark => theme::DARK,
            ThemeChoice::Light => theme::LIGHT,
        }
    }

    fn save(&self) {
        let Some(path) = config_path() else { return };
        let saved = Saved { sessions: self.sessions.iter().rev().take(200).rev().cloned().collect(), theme: self.theme, mode: self.mode, device: self.device.clone(), digits: self.digits };
        if let Some(dir) = path.parent() {
            let _ = std::fs::create_dir_all(dir);
        }
        if let Ok(s) = serde_json::to_string(&saved) {
            let _ = std::fs::write(path, s);
        }
    }

    fn options(&self) -> Options {
        Options { timeout: 25.0, steps: self.mode != Mode::Approximate, digits: if self.mode == Mode::Approximate { self.digits.max(10) } else { self.digits } }
    }

    fn submit(&mut self, ctx: &egui::Context, q: String) {
        let q = q.trim().to_string();
        if q.is_empty() {
            return;
        }
        let id = self.next_id;
        self.next_id += 1;
        self.sessions.push(Session { id, query: q.clone(), created: chrono::Local::now().timestamp(), result: None, running: true, mode_used: Some(self.mode) });
        self.current = Some(self.sessions.len() - 1);
        self.tab = Tab::Answer;
        self.input.clear();
        self.run_query(ctx, id, q);
        self.save();
    }

    fn run_query(&self, ctx: &egui::Context, id: u64, q: String) {
        let tx = self.tx.clone();
        let opts = self.options();
        let ctx = ctx.clone();
        let device = self.device.clone();
        let approx = self.mode == Mode::Approximate;
        std::thread::spawn(move || {
            noesis::compute::set_preference(&device);
            let mut r = noesis::query_with(&q, &opts);
            if approx {
                // promote decimal approximations in approximate mode
                if let Some(pos) = r.pods.iter().position(|p| p.id == "DecimalApproximation") {
                    for p in r.pods.iter_mut() {
                        p.primary = false;
                    }
                    let mut p = r.pods.remove(pos);
                    p.primary = true;
                    let at = r.pods.iter().position(|p| p.id != "Input").unwrap_or(r.pods.len());
                    r.pods.insert(at, p);
                }
            }
            let _ = tx.send((id, r));
            ctx.request_repaint();
        });
    }

    fn rerun_current(&mut self, ctx: &egui::Context) {
        if let Some(i) = self.current {
            let (id, q) = (self.sessions[i].id, self.sessions[i].query.clone());
            self.sessions[i].running = true;
            self.sessions[i].result = None;
            self.sessions[i].mode_used = Some(self.mode);
            self.plots.clear();
            self.run_query(ctx, id, q);
        }
    }

    fn toast(&mut self, msg: impl Into<String>) {
        self.toast = Some((msg.into(), Instant::now()));
    }
}

impl eframe::App for App {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        while let Ok((id, r)) = self.rx.try_recv() {
            if let Some(s) = self.sessions.iter_mut().find(|s| s.id == id) {
                s.result = Some(r);
                s.running = false;
            }
        }
        let pal = self.palette();
        // keyboard shortcuts
        if ctx.input(|i| i.modifiers.command && i.key_pressed(Key::N)) {
            self.current = None;
            self.focus_input = true;
        }
        if ctx.input(|i| i.modifiers.command && i.key_pressed(Key::B)) {
            self.sidebar_open = !self.sidebar_open;
        }
        if self.sidebar_open {
            self.sidebar(ctx, &pal);
        }
        self.header(ctx, &pal);
        self.composer(ctx, &pal);
        self.central(ctx, &pal);
        if self.settings_open {
            self.settings(ctx, &pal);
        }
        if let Some((msg, t)) = &self.toast {
            if t.elapsed().as_secs_f32() > 4.0 {
                self.toast = None;
            } else {
                let msg = msg.clone();
                egui::Area::new("toast".into()).anchor(Align2::CENTER_TOP, vec2(0.0, 70.0)).show(ctx, |ui| {
                    egui::Frame::new().fill(pal.raised).stroke(Stroke::new(1.0_f32, pal.border)).corner_radius(CornerRadius::same(10)).inner_margin(Margin::symmetric(14, 8)).show(ui, |ui| {
                        ui.label(RichText::new(msg).color(pal.text));
                    });
                });
                ctx.request_repaint_after(std::time::Duration::from_millis(250));
            }
        }
        if self.sessions.iter().any(|s| s.running) {
            ctx.request_repaint_after(std::time::Duration::from_millis(60));
        }
    }
}

// ---------------------------------------------------------------------------
// sidebar
// ---------------------------------------------------------------------------

fn rel_time(ts: i64) -> String {
    let d = chrono::Local::now().timestamp() - ts;
    match d {
        d if d < 60 => "now".into(),
        d if d < 3600 => format!("{}m", d / 60),
        d if d < 86400 => format!("{}h", d / 3600),
        d => format!("{}d", d / 86400),
    }
}

fn icon_button(ui: &mut Ui, size: f32, pal: &Palette, draw: impl Fn(&egui::Painter, egui::Pos2, f32, Color32)) -> egui::Response {
    let (rect, resp) = ui.allocate_exact_size(vec2(size, size), Sense::click());
    if resp.hovered() {
        ui.painter().rect_filled(rect, 6.0, pal.hover);
    }
    draw(ui.painter(), rect.center(), size * 0.32, if resp.hovered() { pal.text } else { pal.muted });
    resp
}

impl App {
    fn sidebar(&mut self, ctx: &egui::Context, pal: &Palette) {
        egui::SidePanel::left("sidebar").resizable(false).show_separator_line(false).exact_width(292.0).frame(egui::Frame::new().fill(pal.sidebar).inner_margin(Margin { left: 14, right: 14, top: 14, bottom: 12 })).show(ctx, |ui| {
            let full = ui.max_rect().expand2(vec2(14.0, 14.0));
            ui.painter().line_segment([full.right_top(), full.right_bottom()], Stroke::new(1.0_f32, pal.border));
            // brand row
            ui.horizontal(|ui| {
                let (r, _) = ui.allocate_exact_size(vec2(26.0, 26.0), Sense::hover());
                icons::logo(ui.painter(), r.center(), 12.0, pal.accent, Color32::WHITE);
                ui.add_space(2.0);
                ui.label(RichText::new("noesis").font(FontId::new(21.0, theme::bold())).color(pal.text));
                let chip = egui::Frame::new().fill(pal.raised).stroke(Stroke::new(1.0_f32, pal.border)).corner_radius(CornerRadius::same(4)).inner_margin(Margin::symmetric(5, 1));
                chip.show(ui, |ui| {
                    ui.label(RichText::new("COMPUTE").size(9.5).color(pal.muted).strong());
                });
                ui.with_layout(Layout::right_to_left(Align::Center), |ui| {
                    if icon_button(ui, 28.0, pal, icons::sidebar).on_hover_text("Hide sidebar (Ctrl+B)").clicked() {
                        self.sidebar_open = false;
                    }
                });
            });
            ui.add_space(14.0);
            // new session pill
            let (rect, resp) = ui.allocate_exact_size(vec2(ui.available_width(), 40.0), Sense::click());
            let fill = if resp.hovered() { pal.hover } else { pal.raised };
            ui.painter().rect_filled(rect, 10.0, fill);
            ui.painter().rect_stroke(rect, 10.0, Stroke::new(1.0_f32, pal.border), StrokeKind::Inside);
            let label_w = 118.0;
            icons::plus_circle(ui.painter(), pos2(rect.center().x - label_w / 2.0, rect.center().y), 7.5, pal.text);
            ui.painter().text(pos2(rect.center().x - label_w / 2.0 + 14.0, rect.center().y), Align2::LEFT_CENTER, "New Session", FontId::proportional(15.0), pal.text);
            if resp.clicked() {
                self.current = None;
                self.focus_input = true;
                self.tab = Tab::Answer;
            }
            ui.add_space(16.0);
            ui.horizontal(|ui| {
                ui.label(RichText::new("History").color(pal.muted).size(14.0));
                ui.with_layout(Layout::right_to_left(Align::Center), |ui| {
                    if icon_button(ui, 26.0, pal, icons::search).on_hover_text("Search history").clicked() {
                        self.search_open = !self.search_open;
                        if !self.search_open {
                            self.search.clear();
                        }
                    }
                });
            });
            if self.search_open {
                ui.add(egui::TextEdit::singleline(&mut self.search).hint_text("Filter…").desired_width(f32::INFINITY));
            }
            ui.add_space(4.0);
            let bottom_h = 44.0;
            let list_h = (ui.available_height() - bottom_h).max(60.0);
            egui::ScrollArea::vertical().max_height(list_h).auto_shrink([false, false]).show(ui, |ui| {
                let filter = self.search.to_lowercase();
                let mut clicked: Option<usize> = None;
                let mut delete: Option<usize> = None;
                for idx in (0..self.sessions.len()).rev() {
                    let s = &self.sessions[idx];
                    if !filter.is_empty() && !s.query.to_lowercase().contains(&filter) {
                        continue;
                    }
                    let selected = self.current == Some(idx);
                    let (rect, resp) = ui.allocate_exact_size(vec2(ui.available_width(), 34.0), Sense::click());
                    if selected || resp.hovered() {
                        ui.painter().rect_filled(rect, 8.0, if selected { pal.raised } else { pal.hover.gamma_multiply(0.6) });
                    }
                    let dot = if s.running { pal.accent_soft } else if s.result.as_ref().is_some_and(|r| !r.success) { pal.bad } else { pal.faint };
                    ui.painter().circle_filled(pos2(rect.left() + 12.0, rect.center().y), 2.5, dot);
                    let time = rel_time(s.created);
                    let max_chars = ((rect.width() - 70.0) / 7.6) as usize;
                    let mut title: String = s.query.chars().take(max_chars).collect();
                    if s.query.chars().count() > max_chars {
                        title.push('…');
                    }
                    ui.painter().text(pos2(rect.left() + 24.0, rect.center().y), Align2::LEFT_CENTER, title, FontId::proportional(14.0), pal.text);
                    ui.painter().text(pos2(rect.right() - 8.0, rect.center().y), Align2::RIGHT_CENTER, time, FontId::proportional(12.0), pal.faint);
                    if resp.clicked() {
                        clicked = Some(idx);
                    }
                    resp.context_menu(|ui| {
                        if ui.button("Delete").clicked() {
                            delete = Some(idx);
                            ui.close_menu();
                        }
                    });
                }
                if let Some(i) = clicked {
                    self.current = Some(i);
                    self.tab = Tab::Answer;
                    self.plots.clear();
                    if self.sessions[i].result.is_none() && !self.sessions[i].running {
                        self.sessions[i].running = true;
                        let (id, q) = (self.sessions[i].id, self.sessions[i].query.clone());
                        self.run_query(ui.ctx(), id, q);
                    }
                }
                if let Some(i) = delete {
                    self.sessions.remove(i);
                    self.current = None;
                    self.save();
                }
                if self.sessions.is_empty() {
                    ui.add_space(8.0);
                    ui.label(RichText::new("Your queries will appear here.").color(pal.faint).size(13.0));
                }
            });
            ui.with_layout(Layout::bottom_up(Align::LEFT), |ui| {
                let (rect, resp) = ui.allocate_exact_size(vec2(ui.available_width(), 36.0), Sense::click());
                if resp.hovered() {
                    ui.painter().rect_filled(rect, 8.0, pal.hover.gamma_multiply(0.6));
                }
                icons::gear(ui.painter(), pos2(rect.left() + 16.0, rect.center().y), 8.0, pal.text);
                ui.painter().text(pos2(rect.left() + 34.0, rect.center().y), Align2::LEFT_CENTER, "Settings", FontId::proportional(15.0), pal.text);
                if resp.clicked() {
                    self.settings_open = !self.settings_open;
                }
            });
        });
    }

    // -----------------------------------------------------------------------
    // header
    // -----------------------------------------------------------------------

    fn header(&mut self, ctx: &egui::Context, pal: &Palette) {
        egui::TopBottomPanel::top("header").exact_height(88.0).show_separator_line(false).frame(egui::Frame::new().fill(pal.bg).inner_margin(Margin { left: 26, right: 20, top: 12, bottom: 0 })).show(ctx, |ui| {
            let full = ui.max_rect();
            ui.painter().line_segment([pos2(full.left() - 26.0, full.bottom()), pos2(full.right() + 20.0, full.bottom())], Stroke::new(1.0_f32, pal.border));
            ui.horizontal(|ui| {
                if !self.sidebar_open && icon_button(ui, 28.0, pal, icons::sidebar).on_hover_text("Show sidebar (Ctrl+B)").clicked() {
                    self.sidebar_open = true;
                }
                let title = self.current.map(|i| self.sessions[i].query.clone()).unwrap_or_else(|| "New session".into());
                let short: String = title.chars().take(70).collect();
                ui.label(RichText::new(short).size(16.0).color(pal.text).strong());
                ui.add_space(10.0);
                let (r, _) = ui.allocate_exact_size(vec2(14.0, 14.0), Sense::hover());
                icons::diamond(ui.painter(), r.center(), 5.5, pal.muted);
                let mode = self.current.and_then(|i| self.sessions[i].mode_used).unwrap_or(self.mode);
                ui.label(RichText::new(mode.label()).size(13.5).color(pal.muted));
                ui.with_layout(Layout::right_to_left(Align::Center), |ui| {
                    let enabled = self.current.and_then(|i| self.sessions[i].result.as_ref()).is_some();
                    let btn = egui::Button::new(RichText::new("Export      ").size(14.0).color(pal.text)).fill(pal.bg).stroke(Stroke::new(1.0_f32, pal.border)).corner_radius(CornerRadius::same(18)).min_size(vec2(100.0, 32.0));
                    let resp = ui.add_enabled(enabled, btn);
                    icons::download(ui.painter(), pos2(resp.rect.right() - 22.0, resp.rect.center().y), 6.0, if enabled { pal.text } else { pal.faint });
                    let popup = ui.make_persistent_id("export-menu");
                    if resp.clicked() {
                        ui.memory_mut(|m| m.toggle_popup(popup));
                    }
                    egui::popup_below_widget(ui, popup, &resp, egui::PopupCloseBehavior::CloseOnClick, |ui| {
                        ui.set_min_width(200.0);
                        if ui.button("Copy as text").clicked() {
                            if let Some(r) = self.current.and_then(|i| self.sessions[i].result.clone()) {
                                ui.ctx().copy_text(result_markdown(&r));
                                self.toast("Copied to clipboard");
                            }
                        }
                        if ui.button("Save as Markdown").clicked() {
                            self.export("md");
                        }
                        if ui.button("Save as JSON").clicked() {
                            self.export("json");
                        }
                        if ui.button("Save plots as SVG").clicked() {
                            self.export("svg");
                        }
                    });
                });
            });
            ui.add_space(6.0);
            ui.horizontal(|ui| {
                for (tab, label) in [(Tab::Answer, "Answer"), (Tab::Trace, "Trace")] {
                    let selected = self.tab == tab;
                    let galley = ui.painter().layout_no_wrap(label.to_string(), FontId::proportional(14.5), if selected { pal.accent_soft } else { pal.muted });
                    let (rect, resp) = ui.allocate_exact_size(vec2(galley.size().x + 8.0, 30.0), Sense::click());
                    let col = if selected { pal.accent_soft } else if resp.hovered() { pal.text } else { pal.muted };
                    ui.painter().text(pos2(rect.left() + 4.0, rect.top() + 6.0), Align2::LEFT_TOP, label, FontId::proportional(14.5), col);
                    if selected {
                        ui.painter().line_segment([pos2(rect.left(), rect.bottom() - 1.0), pos2(rect.right(), rect.bottom() - 1.0)], Stroke::new(2.0_f32, pal.accent_soft));
                    }
                    if resp.clicked() {
                        self.tab = tab;
                    }
                    ui.add_space(22.0);
                }
            });
        });
    }

    fn export(&mut self, kind: &str) {
        let Some(r) = self.current.and_then(|i| self.sessions[i].result.clone()) else { return };
        let dir = export_dir();
        let _ = std::fs::create_dir_all(&dir);
        let stem: String = r.query.chars().map(|c| if c.is_alphanumeric() { c } else { '_' }).take(40).collect();
        let stamp = chrono::Local::now().format("%Y%m%d-%H%M%S");
        let result = match kind {
            "md" => {
                let p = dir.join(format!("{stem}-{stamp}.md"));
                std::fs::write(&p, result_markdown(&r)).map(|_| p.display().to_string())
            }
            "json" => {
                let p = dir.join(format!("{stem}-{stamp}.json"));
                std::fs::write(&p, serde_json::to_string_pretty(&r).unwrap_or_default()).map(|_| p.display().to_string())
            }
            _ => {
                let mut n = 0;
                let mut last = String::new();
                for pod in &r.pods {
                    for sp in &pod.subpods {
                        if let Content::Plot { plot } = &sp.content {
                            if plot.grid.is_none() {
                                n += 1;
                                let p = dir.join(format!("{stem}-{stamp}-{n}.svg"));
                                if std::fs::write(&p, noesis::plot::to_svg(plot, self.theme == ThemeChoice::Dark, 720.0, 440.0)).is_ok() {
                                    last = p.display().to_string();
                                }
                            }
                        }
                    }
                }
                if n == 0 {
                    Err(std::io::Error::other("no 2-D plots in this result"))
                } else {
                    Ok(last)
                }
            }
        };
        match result {
            Ok(p) => self.toast(format!("Saved {p}")),
            Err(e) => self.toast(format!("Export failed: {e}")),
        }
    }

    // -----------------------------------------------------------------------
    // composer
    // -----------------------------------------------------------------------

    fn composer(&mut self, ctx: &egui::Context, pal: &Palette) {
        egui::TopBottomPanel::bottom("composer").show_separator_line(false).frame(egui::Frame::new().fill(pal.bg).inner_margin(Margin { left: 20, right: 20, top: 8, bottom: 10 })).show(ctx, |ui| {
            let w = ui.available_width().min(CONTENT_WIDTH);
            let pad = ((ui.available_width() - w) / 2.0).max(0.0);
            ui.horizontal(|ui| {
                ui.add_space(pad);
                ui.vertical(|ui| {
                    ui.set_width(w);
                    let frame = egui::Frame::new().fill(pal.panel).stroke(Stroke::new(1.0_f32, pal.border)).corner_radius(CornerRadius::same(18)).inner_margin(Margin { left: 16, right: 12, top: 12, bottom: 10 });
                    frame.show(ui, |ui| {
                        let running = self.current.is_some_and(|i| self.sessions[i].running);
                        let edit = egui::TextEdit::multiline(&mut self.input)
                            .hint_text(RichText::new("Ask anything: math, science, units, dates…").color(pal.faint))
                            .desired_rows(1)
                            .desired_width(f32::INFINITY)
                            .frame(false)
                            .font(FontId::proportional(15.5))
                            .return_key(Some(egui::KeyboardShortcut::new(egui::Modifiers::SHIFT, Key::Enter)));
                        let out = ui.add(edit);
                        if self.focus_input {
                            out.request_focus();
                            self.focus_input = false;
                        }
                        let enter = out.has_focus() && ui.input(|i| i.key_pressed(Key::Enter) && !i.modifiers.shift);
                        ui.add_space(6.0);
                        let mut send = enter;
                        ui.horizontal(|ui| {
                            // examples
                            let plus = icon_button(ui, 30.0, pal, |p, c, _, col| icons::plus_circle(p, c, 10.0, col)).on_hover_text("Examples");
                            let pid = ui.make_persistent_id("examples");
                            if plus.clicked() {
                                ui.memory_mut(|m| m.toggle_popup(pid));
                            }
                            let mut picked: Option<String> = None;
                            egui::popup_above_or_below_widget(ui, pid, &plus, egui::AboveOrBelow::Above, egui::PopupCloseBehavior::CloseOnClickOutside, |ui| {
                                ui.set_min_width(330.0);
                                egui::ScrollArea::vertical().max_height(380.0).show(ui, |ui| {
                                    for (cat, items) in EXAMPLES {
                                        ui.label(RichText::new(*cat).color(pal.muted).size(12.5));
                                        for it in *items {
                                            if ui.selectable_label(false, *it).clicked() {
                                                picked = Some(it.to_string());
                                                ui.memory_mut(|m| m.close_popup());
                                            }
                                        }
                                        ui.add_space(4.0);
                                    }
                                });
                            });
                            if let Some(p) = picked {
                                self.input = p;
                                send = true;
                            }
                            // mode dropdown
                            ui.add_space(4.0);
                            let (r, _) = ui.allocate_exact_size(vec2(14.0, 14.0), Sense::hover());
                            icons::diamond(ui.painter(), r.center(), 5.0, pal.muted);
                            egui::ComboBox::from_id_salt("mode").selected_text(RichText::new(self.mode.short()).color(pal.text)).width(120.0).show_ui(ui, |ui| {
                                for m in [Mode::Exact, Mode::Approximate, Mode::Steps] {
                                    if ui.selectable_value(&mut self.mode, m, m.short()).changed() {
                                        self.save();
                                    }
                                }
                            });
                            ui.with_layout(Layout::right_to_left(Align::Center), |ui| {
                                // send button
                                let (rect, resp) = ui.allocate_exact_size(vec2(36.0, 36.0), Sense::click());
                                let can_send = !self.input.trim().is_empty();
                                let fill = if can_send || running { pal.accent } else { pal.accent.gamma_multiply(0.45) };
                                ui.painter().circle_filled(rect.center(), 18.0, if resp.hovered() && can_send { pal.accent_soft } else { fill });
                                if running {
                                    icons::stop(ui.painter(), rect.center(), 7.0, Color32::WHITE);
                                } else {
                                    icons::arrow_up(ui.painter(), rect.center(), 8.0, Color32::WHITE);
                                }
                                if resp.on_hover_text("Send (Enter)").clicked() && can_send {
                                    send = true;
                                }
                                ui.add_space(6.0);
                                if running {
                                    ui.add(egui::Spinner::new().size(18.0).color(pal.muted));
                                    ui.add_space(6.0);
                                }
                                // device selector
                                let info = noesis::compute::info();
                                let gpu_name = info.gpu.clone();
                                let dev_label = match self.device.as_str() {
                                    "cpu" => "CPU".to_string(),
                                    "gpu" => "GPU".to_string(),
                                    _ => "Auto".to_string(),
                                };
                                let active = if noesis::compute::active() == noesis::compute::Device::Gpu { "GPU" } else { "CPU" };
                                egui::ComboBox::from_id_salt("device")
                                    .selected_text(RichText::new(format!("{dev_label} · {active}")).color(pal.text))
                                    .width(130.0)
                                    .show_ui(ui, |ui| {
                                        let mut changed = false;
                                        changed |= ui.selectable_value(&mut self.device, "auto".to_string(), "Auto (GPU if available)").changed();
                                        changed |= ui.selectable_value(&mut self.device, "cpu".to_string(), format!("CPU · {} threads", info.cpu_threads)).changed();
                                        let gl = gpu_name.clone().map(|g| format!("GPU · {g}")).unwrap_or_else(|| "GPU · not available".into());
                                        changed |= ui.selectable_value(&mut self.device, "gpu".to_string(), gl).changed();
                                        if changed {
                                            noesis::compute::set_preference(&self.device);
                                            self.save();
                                        }
                                    });
                                let (r, _) = ui.allocate_exact_size(vec2(16.0, 16.0), Sense::hover());
                                icons::chip(ui.painter(), r.center(), 6.0, pal.muted);
                            });
                        });
                        if send && !running {
                            let q = self.input.trim_end_matches('\n').to_string();
                            self.submit(ui.ctx(), q);
                        }
                    });
                    // stats line
                    ui.add_space(5.0);
                    let stats = self.stats_line();
                    ui.vertical_centered(|ui| {
                        ui.label(RichText::new(stats).size(12.0).color(pal.faint));
                    });
                });
            });
        });
    }

    fn stats_line(&self) -> String {
        let info = noesis::compute::info();
        let dev = match noesis::compute::active() {
            noesis::compute::Device::Gpu => format!("GPU {}", info.gpu.clone().unwrap_or_default()),
            noesis::compute::Device::Cpu => format!("CPU {} threads", info.cpu_threads),
        };
        match self.current.and_then(|i| self.sessions[i].result.as_ref()) {
            Some(r) => {
                let plots = r.pods.iter().flat_map(|p| &p.subpods).filter(|s| matches!(s.content, Content::Plot { .. })).count();
                format!("{} pods  ·  {} plots  ·  {:.0} ms  ·  {}  ·  {} sessions", r.pods.len(), plots, r.timing_ms, dev, self.sessions.len())
            }
            None => format!("{dev}  ·  {} sessions  ·  Enter to send, Shift+Enter for a new line", self.sessions.len()),
        }
    }

    // -----------------------------------------------------------------------
    // central area
    // -----------------------------------------------------------------------

    fn central(&mut self, ctx: &egui::Context, pal: &Palette) {
        egui::CentralPanel::default().frame(egui::Frame::new().fill(pal.bg).inner_margin(Margin::symmetric(20, 0))).show(ctx, |ui| {
            let Some(idx) = self.current else {
                self.welcome(ui, pal);
                return;
            };
            let session = self.sessions[idx].clone();
            egui::ScrollArea::vertical().auto_shrink([false, false]).show(ui, |ui| {
                let w = ui.available_width().min(CONTENT_WIDTH);
                let pad = ((ui.available_width() - w) / 2.0).max(0.0);
                ui.horizontal(|ui| {
                    ui.add_space(pad);
                    ui.vertical(|ui| {
                        ui.set_width(w);
                        ui.add_space(18.0);
                        self.user_bubble(ui, pal, &session);
                        ui.add_space(8.0);
                        match (&session.result, self.tab) {
                            (None, _) => {
                                ui.horizontal(|ui| {
                                    ui.add(egui::Spinner::new().size(16.0).color(pal.muted));
                                    ui.label(RichText::new("Computing…").color(pal.muted));
                                });
                            }
                            (Some(r), Tab::Answer) => self.answer(ui, pal, r),
                            (Some(r), Tab::Trace) => self.trace(ui, pal, r),
                        }
                        ui.add_space(30.0);
                    });
                });
            });
        });
    }

    fn user_bubble(&mut self, ui: &mut Ui, pal: &Palette, s: &Session) {
        ui.with_layout(Layout::right_to_left(Align::TOP), |ui| {
            let max_w = ui.available_width() * 0.75;
            egui::Frame::new().fill(pal.bubble).corner_radius(CornerRadius::same(20)).inner_margin(Margin::symmetric(16, 10)).show(ui, |ui| {
                ui.set_max_width(max_w);
                ui.label(RichText::new(&s.query).size(15.5).color(pal.text));
            });
        });
        ui.with_layout(Layout::right_to_left(Align::Center), |ui| {
            ui.add_space(14.0);
            if icon_button(ui, 24.0, pal, icons::copy).on_hover_text("Copy query").clicked() {
                ui.ctx().copy_text(s.query.clone());
                self.toast("Query copied");
            }
            ui.add_space(4.0);
            let t = chrono::DateTime::from_timestamp(s.created, 0).map(|d| d.with_timezone(&chrono::Local).format("%H:%M").to_string()).unwrap_or_default();
            ui.label(RichText::new(t).size(13.0).color(pal.faint));
        });
    }

    fn welcome(&mut self, ui: &mut Ui, pal: &Palette) {
        egui::ScrollArea::vertical().auto_shrink([false, false]).show(ui, |ui| {
            let w = ui.available_width().min(CONTENT_WIDTH);
            let pad = ((ui.available_width() - w) / 2.0).max(0.0);
            ui.horizontal(|ui| {
                ui.add_space(pad);
                ui.vertical(|ui| {
                    ui.set_width(w);
                    ui.add_space((ui.available_height() * 0.12).clamp(20.0, 90.0));
                    ui.vertical_centered(|ui| {
                        let (r, _) = ui.allocate_exact_size(vec2(56.0, 56.0), Sense::hover());
                        icons::logo(ui.painter(), r.center(), 26.0, pal.accent, Color32::WHITE);
                        ui.add_space(10.0);
                        ui.label(RichText::new("What would you like to compute?").size(26.0).color(pal.text));
                        ui.add_space(4.0);
                        ui.label(RichText::new("Exact symbolic math, step-by-step solutions, plots on your GPU or CPU, and everyday knowledge, offline.").size(14.5).color(pal.muted));
                    });
                    ui.add_space(26.0);
                    let mut picked = None;
                    let cols = if w > 640.0 { 2 } else { 1 };
                    let card = |ui: &mut Ui, cat: &str, items: &[&str], picked: &mut Option<String>| {
                        egui::Frame::new().fill(pal.panel).stroke(Stroke::new(1.0_f32, pal.border)).corner_radius(CornerRadius::same(14)).inner_margin(Margin::same(14)).show(ui, |ui| {
                            ui.set_width(ui.available_width());
                            ui.vertical(|ui| {
                                ui.label(RichText::new(cat).size(13.0).color(pal.muted));
                                ui.add_space(4.0);
                                for it in items.iter() {
                                    let resp = ui.add(egui::Label::new(RichText::new(*it).size(14.5).color(pal.text)).sense(Sense::click()).truncate());
                                    if resp.hovered() {
                                        ui.painter().line_segment([resp.rect.left_bottom(), resp.rect.right_bottom()], Stroke::new(1.0_f32, pal.accent_soft));
                                    }
                                    if resp.clicked() {
                                        *picked = Some(it.to_string());
                                    }
                                }
                            });
                        });
                        ui.add_space(12.0);
                    };
                    ui.columns(cols, |columns| {
                        for (k, (cat, items)) in EXAMPLES.iter().enumerate() {
                            card(&mut columns[k % cols], cat, items, &mut picked);
                        }
                    });
                    if let Some(p) = picked {
                        self.submit(ui.ctx(), p);
                    }
                });
            });
        });
    }

    fn answer(&mut self, ui: &mut Ui, pal: &Palette, r: &QueryResult) {
        if !r.success {
            egui::Frame::new().fill(pal.panel).stroke(Stroke::new(1.0_f32, pal.border)).corner_radius(CornerRadius::same(12)).inner_margin(Margin::same(14)).show(ui, |ui| {
                ui.set_width(ui.available_width());
                ui.label(RichText::new(r.error.clone().unwrap_or_else(|| "No result.".into())).color(pal.text).size(15.0));
                if !r.suggestions.is_empty() {
                    ui.add_space(6.0);
                    ui.label(RichText::new("Try one of these:").color(pal.muted));
                    let mut picked = None;
                    ui.horizontal_wrapped(|ui| {
                        for s in &r.suggestions {
                            if ui.button(s).clicked() {
                                picked = Some(s.clone());
                            }
                        }
                    });
                    if let Some(p) = picked {
                        self.submit(ui.ctx(), p);
                    }
                }
            });
            return;
        }
        if let Some(interp) = &r.interpretation {
            meta_row(ui, pal, "Interpretation", interp);
        }
        meta_row(ui, pal, "Compute", &format!("{} · {:.0} ms", r.device, r.timing_ms));
        ui.add_space(8.0);
        for (k, pod) in r.pods.iter().enumerate() {
            if pod.id == "Input" && k == 0 {
                self.pod_view(ui, pal, pod, k, true);
                continue;
            }
            self.pod_view(ui, pal, pod, k, false);
        }
    }

    fn pod_view(&mut self, ui: &mut Ui, pal: &Palette, pod: &Pod, k: usize, compact: bool) {
        ui.add_space(if compact { 2.0 } else { 10.0 });
        ui.horizontal(|ui| {
            let (r, _) = ui.allocate_exact_size(vec2(16.0, 18.0), Sense::hover());
            icons::doc(ui.painter(), r.center(), 6.0, if pod.primary { pal.accent_soft } else { pal.muted });
            let title = RichText::new(&pod.title).size(14.0).color(if pod.primary { pal.accent_soft } else { pal.muted });
            ui.label(if pod.primary { title.strong() } else { title });
        });
        ui.add_space(3.0);
        let indent = 24.0;
        for (j, sp) in pod.subpods.iter().enumerate() {
            ui.horizontal(|ui| {
                ui.add_space(indent);
                ui.vertical(|ui| {
                    if !sp.title.is_empty() {
                        ui.label(RichText::new(&sp.title).size(12.5).color(pal.faint));
                    }
                    let size = if pod.primary { 19.0 } else { 16.5 };
                    content_view(ui, pal, &sp.content, size, egui::Id::new(("pod", &pod.id, k, j)), &mut self.plots);
                });
            });
        }
        if !compact {
            ui.add_space(4.0);
            let (rect, _) = ui.allocate_exact_size(vec2(ui.available_width(), 1.0), Sense::hover());
            ui.painter().line_segment([rect.left_center(), rect.right_center()], Stroke::new(1.0_f32, pal.border.gamma_multiply(0.6)));
        }
    }

    fn trace(&mut self, ui: &mut Ui, pal: &Palette, r: &QueryResult) {
        meta_row(ui, pal, "Query", &r.query);
        if let Some(i) = &r.interpretation {
            meta_row(ui, pal, "Interpreted as", i);
        }
        meta_row(ui, pal, "Outcome", if r.success { "answered" } else { "not understood" });
        meta_row(ui, pal, "Compute", &format!("{}", r.device));
        meta_row(ui, pal, "Wall time", &format!("{:.1} ms", r.timing_ms));
        for a in &r.assumptions {
            meta_row(ui, pal, "Assumption", a);
        }
        ui.add_space(10.0);
        ui.label(RichText::new("Pods produced").color(pal.muted));
        egui::Grid::new("trace-grid").striped(true).num_columns(3).spacing(vec2(18.0, 6.0)).show(ui, |ui| {
            ui.label(RichText::new("#").color(pal.faint));
            ui.label(RichText::new("id").color(pal.faint));
            ui.label(RichText::new("title").color(pal.faint));
            ui.end_row();
            for (k, p) in r.pods.iter().enumerate() {
                ui.label(RichText::new(format!("{}", k + 1)).color(pal.faint));
                ui.label(RichText::new(&p.id).monospace().color(pal.text));
                ui.label(RichText::new(format!("{}{}", p.title, if p.primary { "  (primary)" } else { "" })).color(pal.text));
                ui.end_row();
            }
        });
        // worked steps, if any
        let steps: Vec<&Pod> = r.pods.iter().filter(|p| p.subpods.iter().any(|s| matches!(s.content, Content::Steps { .. }))).collect();
        for p in steps {
            ui.add_space(12.0);
            ui.label(RichText::new(&p.title).color(pal.muted));
            for sp in &p.subpods {
                content_view(ui, pal, &sp.content, 16.0, egui::Id::new(("trace", &p.id)), &mut self.plots);
            }
        }
        ui.add_space(12.0);
        ui.horizontal(|ui| {
            if ui.button("Copy JSON").clicked() {
                ui.ctx().copy_text(serde_json::to_string_pretty(r).unwrap_or_default());
                self.toast("JSON copied");
            }
            if ui.button("Re-run").clicked() {
                self.rerun_current(ui.ctx());
            }
        });
    }

    // -----------------------------------------------------------------------
    // settings
    // -----------------------------------------------------------------------

    fn settings(&mut self, ctx: &egui::Context, pal: &Palette) {
        let mut open = true;
        egui::Window::new(RichText::new("Settings").size(16.0)).open(&mut open).collapsible(false).resizable(false).default_width(420.0).anchor(Align2::CENTER_CENTER, vec2(0.0, 0.0)).show(ctx, |ui| {
            ui.add_space(4.0);
            ui.label(RichText::new("Appearance").color(pal.muted));
            ui.horizontal(|ui| {
                let mut changed = false;
                changed |= ui.selectable_value(&mut self.theme, ThemeChoice::Dark, "Dark").changed();
                changed |= ui.selectable_value(&mut self.theme, ThemeChoice::Light, "Light").changed();
                if changed {
                    theme::apply(ctx, &self.palette());
                    self.plots.clear();
                    self.save();
                }
            });
            ui.add_space(10.0);
            ui.label(RichText::new("Results").color(pal.muted));
            ui.horizontal(|ui| {
                ui.label("Decimal digits");
                if ui.add(egui::Slider::new(&mut self.digits, 3..=20)).changed() {
                    self.save();
                }
            });
            ui.horizontal(|ui| {
                ui.label("Default mode");
                for m in [Mode::Exact, Mode::Approximate, Mode::Steps] {
                    if ui.selectable_value(&mut self.mode, m, m.short()).changed() {
                        self.save();
                    }
                }
            });
            ui.add_space(10.0);
            ui.label(RichText::new("Compute devices").color(pal.muted));
            let info = noesis::compute::info();
            egui::Grid::new("devices").num_columns(2).spacing(vec2(16.0, 4.0)).show(ui, |ui| {
                ui.label("CPU");
                ui.label(format!("{} worker threads", info.cpu_threads));
                ui.end_row();
                ui.label("GPU");
                ui.label(match (&info.gpu, &info.gpu_backend) {
                    (Some(g), Some(b)) => format!("{g} ({b})"),
                    _ => "none detected (dense work runs on the CPU)".into(),
                });
                ui.end_row();
                ui.label("Active");
                ui.label(noesis::compute::device_label());
                ui.end_row();
            });
            ui.horizontal(|ui| {
                let mut changed = false;
                changed |= ui.selectable_value(&mut self.device, "auto".to_string(), "Auto").changed();
                changed |= ui.selectable_value(&mut self.device, "cpu".to_string(), "CPU").changed();
                changed |= ui.selectable_value(&mut self.device, "gpu".to_string(), "GPU").changed();
                if changed {
                    noesis::compute::set_preference(&self.device);
                    self.save();
                }
            });
            if ui.button("Run benchmark").clicked() {
                self.bench = Some(noesis::compute::benchmark());
            }
            if let Some(b) = &self.bench {
                for (d, rate) in b {
                    ui.label(RichText::new(format!("{d}: {:.1} M evaluations/s", rate / 1e6)).color(pal.text));
                }
            }
            ui.add_space(10.0);
            ui.label(RichText::new("Data").color(pal.muted));
            ui.horizontal(|ui| {
                if ui.button("Clear history").clicked() {
                    self.sessions.clear();
                    self.current = None;
                    self.save();
                }
                ui.label(RichText::new(format!("exports go to {}", export_dir().display())).size(12.0).color(pal.faint));
            });
            ui.add_space(6.0);
            ui.label(RichText::new(format!("Noesis {} · MIT license · Ctrl+N new session, Ctrl+B sidebar", env!("CARGO_PKG_VERSION"))).size(12.0).color(pal.faint));
        });
        if !open {
            self.settings_open = false;
        }
    }
}

fn meta_row(ui: &mut Ui, pal: &Palette, label: &str, value: &str) {
    ui.horizontal_wrapped(|ui| {
        let (r, _) = ui.allocate_exact_size(vec2(16.0, 18.0), Sense::hover());
        icons::doc(ui.painter(), r.center(), 6.0, pal.faint);
        ui.label(RichText::new(label).size(14.0).color(pal.muted));
        ui.label(RichText::new("·").color(pal.faint));
        ui.label(RichText::new(value).size(14.0).color(pal.muted));
    });
}

fn content_view(ui: &mut Ui, pal: &Palette, c: &Content, size: f32, id: egui::Id, plots: &mut plotview::States) {
    match c {
        Content::Math { node, text, .. } => {
            let avail = ui.available_width();
            let m = mathview::measure(ui, node, size);
            if m.x <= avail {
                let resp = mathview::show(ui, node, size, pal.text);
                resp.on_hover_text("Click to copy").clicked().then(|| ui.ctx().copy_text(text.clone()));
            } else {
                ui.add(egui::Label::new(RichText::new(text).size(size * 0.85).color(pal.text)).wrap());
            }
        }
        Content::Text { text } => {
            ui.add(egui::Label::new(RichText::new(text).size(15.0).color(pal.text)).wrap());
        }
        Content::Table { header, rows } => {
            let ncol = rows.iter().map(|r| r.len()).max().unwrap_or(1).max(header.as_ref().map_or(0, |h| h.len()));
            egui::Grid::new(id.with("table")).num_columns(ncol).spacing(vec2(22.0, 7.0)).show(ui, |ui| {
                if let Some(h) = header {
                    for cell in h {
                        ui.label(RichText::new(cell).size(13.0).color(pal.faint));
                    }
                    ui.end_row();
                }
                for (ri, row) in rows.iter().enumerate() {
                    for (ci, cell) in row.iter().enumerate() {
                        match cell {
                            Content::Text { text } if ci == 0 && ncol > 1 && header.is_none() => {
                                ui.label(RichText::new(text).size(14.5).color(pal.muted));
                            }
                            other => {
                                ui.vertical(|ui| content_view(ui, pal, other, size * 0.9, id.with((ri, ci)), plots));
                            }
                        }
                    }
                    ui.end_row();
                }
            });
        }
        Content::Plot { plot } => {
            ui.add_space(4.0);
            plotview::show(ui, id, plot, pal, plots);
            ui.add_space(4.0);
        }
        Content::Steps { steps } => {
            for (k, s) in steps.iter().enumerate() {
                ui.horizontal(|ui| {
                    ui.add_space(14.0 * s.depth as f32);
                    let (r, _) = ui.allocate_exact_size(vec2(18.0, 18.0), Sense::hover());
                    ui.painter().circle_filled(r.center(), 8.5, pal.raised);
                    ui.painter().text(r.center(), Align2::CENTER_CENTER, format!("{}", k + 1), FontId::proportional(10.0), pal.muted);
                    ui.vertical(|ui| {
                        if !s.text.is_empty() {
                            ui.add(egui::Label::new(RichText::new(&s.text).size(14.0).color(pal.text)).wrap());
                        }
                        if let Some(m) = &s.math {
                            content_view(ui, pal, m, 16.0, id.with(("step", k)), plots);
                        }
                    });
                });
                ui.add_space(2.0);
            }
        }
    }
}

/// Markdown rendering of a result (for export and copying).
pub fn result_markdown(r: &QueryResult) -> String {
    let mut out = format!("# {}\n\n", r.query);
    if let Some(i) = &r.interpretation {
        out += &format!("_Interpreted as:_ `{i}`\n\n");
    }
    for p in &r.pods {
        out += &format!("## {}\n\n", p.title);
        for sp in &p.subpods {
            if !sp.title.is_empty() {
                out += &format!("_{}_\n\n", sp.title);
            }
            match &sp.content {
                Content::Math { text, latex, .. } => out += &format!("$$ {latex} $$\n\n`{text}`\n\n"),
                Content::Table { header, rows } => {
                    let ncol = rows.iter().map(|r| r.len()).max().unwrap_or(1);
                    let h: Vec<String> = header.clone().unwrap_or_else(|| vec![String::new(); ncol]);
                    out += &format!("| {} |\n|{}|\n", h.join(" | "), vec!["---"; h.len().max(ncol)].join("|"));
                    for row in rows {
                        out += &format!("| {} |\n", row.iter().map(content_text).collect::<Vec<_>>().join(" | "));
                    }
                    out += "\n";
                }
                other => out += &format!("{}\n\n", content_text(other)),
            }
        }
    }
    out += &format!("---\nComputed by Noesis in {:.0} ms on {}.\n", r.timing_ms, r.device);
    out
}

#[allow(dead_code)]
fn unused(_: Rect) {}
