//! Colours, fonts and widget styling.  A dark charcoal theme modelled on
//! modern agent workbenches, plus a light variant.

use eframe::egui::{self, Color32, CornerRadius, FontData, FontDefinitions, FontFamily, FontId, Stroke, TextStyle, Visuals};
use std::sync::Arc;

#[derive(Clone, Copy, Debug)]
pub struct Palette {
    pub dark: bool,
    pub bg: Color32,
    pub sidebar: Color32,
    pub panel: Color32,
    pub raised: Color32,
    pub hover: Color32,
    pub border: Color32,
    pub text: Color32,
    pub muted: Color32,
    pub faint: Color32,
    pub accent: Color32,
    pub accent_soft: Color32,
    pub bubble: Color32,
    pub code_bg: Color32,
    pub plot_bg: Color32,
    pub grid: Color32,
    pub good: Color32,
    pub bad: Color32,
}

pub const DARK: Palette = Palette {
    dark: true,
    bg: Color32::from_rgb(0x1b, 0x1b, 0x1c),
    sidebar: Color32::from_rgb(0x16, 0x16, 0x17),
    panel: Color32::from_rgb(0x22, 0x22, 0x24),
    raised: Color32::from_rgb(0x2a, 0x2a, 0x2c),
    hover: Color32::from_rgb(0x30, 0x30, 0x33),
    border: Color32::from_rgb(0x3a, 0x3a, 0x3d),
    text: Color32::from_rgb(0xec, 0xec, 0xec),
    muted: Color32::from_rgb(0xa3, 0xa3, 0xa8),
    faint: Color32::from_rgb(0x6f, 0x6f, 0x75),
    accent: Color32::from_rgb(0x4f, 0x7c, 0xf0),
    accent_soft: Color32::from_rgb(0x6b, 0x95, 0xff),
    bubble: Color32::from_rgb(0x30, 0x30, 0x32),
    code_bg: Color32::from_rgb(0x2b, 0x2b, 0x2e),
    plot_bg: Color32::from_rgb(0x1a, 0x1a, 0x19),
    grid: Color32::from_rgb(0x2e, 0x2e, 0x2c),
    good: Color32::from_rgb(0x19, 0x9e, 0x70),
    bad: Color32::from_rgb(0xe6, 0x67, 0x67),
};

pub const LIGHT: Palette = Palette {
    dark: false,
    bg: Color32::from_rgb(0xfc, 0xfc, 0xfb),
    sidebar: Color32::from_rgb(0xf3, 0xf3, 0xf1),
    panel: Color32::from_rgb(0xff, 0xff, 0xff),
    raised: Color32::from_rgb(0xf0, 0xf0, 0xee),
    hover: Color32::from_rgb(0xe7, 0xe7, 0xe4),
    border: Color32::from_rgb(0xdc, 0xdc, 0xd8),
    text: Color32::from_rgb(0x0b, 0x0b, 0x0b),
    muted: Color32::from_rgb(0x52, 0x51, 0x4e),
    faint: Color32::from_rgb(0x8c, 0x8b, 0x86),
    accent: Color32::from_rgb(0x2a, 0x63, 0xd6),
    accent_soft: Color32::from_rgb(0x2a, 0x78, 0xd6),
    bubble: Color32::from_rgb(0xec, 0xec, 0xe9),
    code_bg: Color32::from_rgb(0xf1, 0xf1, 0xee),
    plot_bg: Color32::from_rgb(0xfc, 0xfc, 0xfb),
    grid: Color32::from_rgb(0xe4, 0xe3, 0xdf),
    good: Color32::from_rgb(0x1b, 0xaf, 0x7a),
    bad: Color32::from_rgb(0xe3, 0x49, 0x48),
};

pub const SERIES_DARK: [Color32; 8] = [
    Color32::from_rgb(0x39, 0x87, 0xe5),
    Color32::from_rgb(0xd9, 0x59, 0x26),
    Color32::from_rgb(0x19, 0x9e, 0x70),
    Color32::from_rgb(0xc9, 0x85, 0x00),
    Color32::from_rgb(0xd5, 0x51, 0x81),
    Color32::from_rgb(0x00, 0x83, 0x00),
    Color32::from_rgb(0x90, 0x85, 0xe9),
    Color32::from_rgb(0xe6, 0x67, 0x67),
];

pub const SERIES_LIGHT: [Color32; 8] = [
    Color32::from_rgb(0x2a, 0x78, 0xd6),
    Color32::from_rgb(0xeb, 0x68, 0x34),
    Color32::from_rgb(0x1b, 0xaf, 0x7a),
    Color32::from_rgb(0xed, 0xa1, 0x00),
    Color32::from_rgb(0xe8, 0x7b, 0xa4),
    Color32::from_rgb(0x00, 0x83, 0x00),
    Color32::from_rgb(0x62, 0x50, 0xd6),
    Color32::from_rgb(0xe3, 0x49, 0x48),
];

impl Palette {
    pub fn series(&self, k: usize) -> Color32 {
        if self.dark {
            SERIES_DARK[k % 8]
        } else {
            SERIES_LIGHT[k % 8]
        }
    }
}

pub fn math_italic() -> FontFamily {
    FontFamily::Name(Arc::from("math-italic"))
}

pub fn bold() -> FontFamily {
    FontFamily::Name(Arc::from("bold"))
}

pub fn install_fonts(ctx: &egui::Context) {
    let mut fonts = FontDefinitions::default();
    fonts.font_data.insert("dejavu".into(), Arc::new(FontData::from_static(include_bytes!("../../assets/fonts/DejaVuSans.ttf"))));
    fonts.font_data.insert("dejavu-bold".into(), Arc::new(FontData::from_static(include_bytes!("../../assets/fonts/DejaVuSans-Bold.ttf"))));
    fonts.font_data.insert("serif-italic".into(), Arc::new(FontData::from_static(include_bytes!("../../assets/fonts/LiberationSerif-Italic.ttf"))));
    fonts.font_data.insert("dejavu-mono".into(), Arc::new(FontData::from_static(include_bytes!("../../assets/fonts/DejaVuSansMono.ttf"))));
    let prop = fonts.families.entry(FontFamily::Proportional).or_default();
    prop.insert(0, "dejavu".into());
    let mono = fonts.families.entry(FontFamily::Monospace).or_default();
    mono.insert(0, "dejavu-mono".into());
    fonts.families.insert(math_italic(), vec!["serif-italic".into(), "dejavu".into()]);
    fonts.families.insert(bold(), vec!["dejavu-bold".into(), "dejavu".into()]);
    ctx.set_fonts(fonts);
}

pub fn apply(ctx: &egui::Context, p: &Palette) {
    let mut v = if p.dark { Visuals::dark() } else { Visuals::light() };
    v.panel_fill = p.bg;
    v.window_fill = p.panel;
    v.extreme_bg_color = p.raised;
    v.faint_bg_color = p.raised;
    v.override_text_color = Some(p.text);
    v.window_stroke = Stroke::new(1.0, p.border);
    v.window_corner_radius = CornerRadius::same(12);
    v.menu_corner_radius = CornerRadius::same(10);
    v.selection.bg_fill = p.accent.gamma_multiply(0.45);
    v.selection.stroke = Stroke::new(1.0, p.accent_soft);
    v.hyperlink_color = p.accent_soft;
    for w in [&mut v.widgets.inactive, &mut v.widgets.hovered, &mut v.widgets.active, &mut v.widgets.open] {
        w.corner_radius = CornerRadius::same(8);
    }
    v.widgets.noninteractive.bg_fill = p.panel;
    v.widgets.noninteractive.bg_stroke = Stroke::new(1.0, p.border);
    v.widgets.noninteractive.fg_stroke = Stroke::new(1.0, p.text);
    v.widgets.inactive.bg_fill = p.raised;
    v.widgets.inactive.weak_bg_fill = p.raised;
    v.widgets.inactive.bg_stroke = Stroke::new(1.0, p.border);
    v.widgets.inactive.fg_stroke = Stroke::new(1.0, p.text);
    v.widgets.hovered.bg_fill = p.hover;
    v.widgets.hovered.weak_bg_fill = p.hover;
    v.widgets.hovered.bg_stroke = Stroke::new(1.0, p.border);
    v.widgets.hovered.fg_stroke = Stroke::new(1.0, p.text);
    v.widgets.active.bg_fill = p.hover;
    v.widgets.active.weak_bg_fill = p.hover;
    v.widgets.active.fg_stroke = Stroke::new(1.0, p.text);
    v.popup_shadow = egui::epaint::Shadow { offset: [0, 4], blur: 16, spread: 0, color: Color32::from_black_alpha(if p.dark { 120 } else { 40 }) };
    ctx.set_visuals(v);
    let mut style = (*ctx.style()).clone();
    style.text_styles = [
        (TextStyle::Heading, FontId::new(17.0, FontFamily::Proportional)),
        (TextStyle::Body, FontId::new(14.5, FontFamily::Proportional)),
        (TextStyle::Monospace, FontId::new(13.5, FontFamily::Monospace)),
        (TextStyle::Button, FontId::new(14.0, FontFamily::Proportional)),
        (TextStyle::Small, FontId::new(12.0, FontFamily::Proportional)),
    ]
    .into();
    style.spacing.item_spacing = egui::vec2(8.0, 6.0);
    style.spacing.button_padding = egui::vec2(10.0, 5.0);
    style.spacing.scroll.bar_width = 6.0;
    style.spacing.scroll.floating = true;
    ctx.set_style(style);
}
