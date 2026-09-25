do
	local env = (typeof(getgenv) == "function" and getgenv()) or _G
	local ok, previous = pcall(function()
		return env.__OuroborosMergeHackers
	end)
	if ok and type(previous) == "table" and type(previous.Unload) == "function" then
		local okSnapshot, snapshot = pcall(function()
			if type(previous.Snapshot) == "function" then
				return previous.Snapshot()
			end
			return nil
		end)
		env.__OuroborosMergeHackersHandover = okSnapshot and snapshot or nil
		pcall(previous.Unload, true)
	end
end

if not game:IsLoaded() then
	game.Loaded:Wait()
end

local LibraryRepo = "https://raw.githubusercontent.com/joustingmatch/ObsidianUltra/main/"
local SCRIPT_FILE = "MergeHackers_Ouroboros_Autofarm.luau"
local SCRIPT_URL = "https://raw.githubusercontent.com/exiletium/script330601/main/MergeHackers_Autofarm.luau"

local function fetchModule(path)
	local ok, result = pcall(function()
		return loadstring(game:HttpGet(LibraryRepo .. path))()
	end)
	if not ok then
		warn("[Ouroboros] could not load " .. path .. ": " .. tostring(result))
		return nil
	end
	return result
end

local Library = fetchModule("Library.lua")
local ThemeManager = fetchModule("addons/ThemeManager.lua")
local SaveManager = fetchModule("addons/SaveManager.lua")
if not Library or not ThemeManager or not SaveManager then
	return
end

local Players = game:GetService("Players")
local ReplicatedStorage = game:GetService("ReplicatedStorage")
local TeleportService = game:GetService("TeleportService")
local VirtualUser = game:GetService("VirtualUser")
local GuiService = game:GetService("GuiService")
local CoreGui = game:GetService("CoreGui")
local Lighting = game:GetService("Lighting")
local Workspace = game:GetService("Workspace")
local RunService = game:GetService("RunService")
local MarketplaceService = game:GetService("MarketplaceService")
local CollectionService = game:GetService("CollectionService")

local LocalPlayer = Players.LocalPlayer
local GlobalEnv = (typeof(getgenv) == "function" and getgenv()) or _G
local BaseIdentity = typeof(getthreadidentity) == "function" and getthreadidentity() or nil
local Handover = GlobalEnv.__OuroborosMergeHackersHandover
GlobalEnv.__OuroborosMergeHackersHandover = nil

local function callable(value)
	return type(value) == "function"
end

local function queueSelf()
	pcall(function()
		local queue = (callable(queue_on_teleport) and queue_on_teleport) or (callable(queueonteleport) and queueonteleport) or nil
		if not queue then
			return
		end
		if callable(isfile) and isfile(SCRIPT_FILE) then
			queue('loadstring(readfile("' .. SCRIPT_FILE .. '"))()')
		else
			queue('loadstring(game:HttpGet("' .. SCRIPT_URL .. '"))()')
		end
	end)
end

local function restoreIdentity()
	if BaseIdentity and typeof(setthreadidentity) == "function" then
		pcall(setthreadidentity, BaseIdentity)
	end
end

local LabelCache = setmetatable({}, { __mode = "k" })

local function setLabel(label, text)
	if LabelCache[label] == text then
		return
	end
	LabelCache[label] = text
	label:SetText(text)
end

local Farm = {
	Running = true,
	Connections = {},
	Cleanup = {},
	Threads = {},
	StartedAt = os.clock(),
	LastError = nil,
	ErrorAt = 0,
}

local Pads = {}
local Carry: { [string]: any } = {}

function Farm.Snapshot()
	local held = {}
	for name, info in Pads do
		if info.Held then
			held[name] = true
		end
	end
	local snapshot = { Held = held }
	if type(Carry.Snapshot) == "function" then
		local ok, extra = pcall(Carry.Snapshot)
		if ok and type(extra) == "table" then
			for key, value in extra do
				snapshot[key] = value
			end
		end
	end
	return snapshot
end

function Farm.Unload(handover)
	if not Farm.Running then
		return
	end
	Farm.Running = false
	restoreIdentity()
	for _, connection in Farm.Connections do
		pcall(function()
			connection:Disconnect()
		end)
	end
	table.clear(Farm.Connections)
	local current = coroutine.running()
	for _, thread in Farm.Threads do
		if thread ~= current and coroutine.status(thread) ~= "dead" then
			pcall(task.cancel, thread)
		end
	end
	table.clear(Farm.Threads)
	for _, fn in Farm.Cleanup do
		pcall(fn, handover == true)
	end
	table.clear(Farm.Cleanup)
	if GlobalEnv.__OuroborosMergeHackers == Farm then
		GlobalEnv.__OuroborosMergeHackers = nil
	end
	if not Library.Unloaded then
		pcall(function()
			Library:Unload()
		end)
	end
end

GlobalEnv.__OuroborosMergeHackers = Farm
Library:OnUnload(function()
	Farm.Unload(false)
end)

local function fail(err)
	Farm.LastError = tostring(err)
	Farm.ErrorAt = os.clock()
end

local function spawnLoop(fn)
	local thread = task.spawn(fn)
	table.insert(Farm.Threads, thread)
	return thread
end

local function connect(signal, fn)
	local connection = signal:Connect(function(...)
		local ok, err = pcall(fn, ...)
		if not ok then
			fail(err)
		end
	end)
	table.insert(Farm.Connections, connection)
	return connection
end

local function notify(title, text, duration)
	restoreIdentity()
	pcall(function()
		Library:Notify({ Title = title, Description = text, Time = duration or 5 })
	end)
end

local Remotes = ReplicatedStorage:WaitForChild("Remotes", 30)
local Shared = ReplicatedStorage:WaitForChild("Shared", 30)
local REMOTE_NAMES = {
	"CurrencyUpdate",
	"RequestSync",
	"RebirthUpgrades",
	"Potions",
	"Quests",
	"Fx",
	"ConverterUpdate",
	"ButtonState",
	"CardSpawn",
	"Milestones",
	"OfflineRewards",
	"Rebirth",
	"GoHome",
	"GoQuestPotions",
	"PlotAssigned",
}
local Net = {}
local Supported = Remotes ~= nil and Shared ~= nil
if Supported then
	for _, name in REMOTE_NAMES do
		local remote = Remotes:WaitForChild(name, 10)
		if not remote or not remote:IsA("RemoteEvent") then
			Supported = false
			break
		end
		Net[name] = remote
	end
end

local GameConfig = nil
local UpgradeRules = nil
if Supported then
	local okConfig, config = pcall(require, Shared:WaitForChild("Config", 10))
	local okRules, rules = pcall(require, Shared:WaitForChild("UpgradeRules", 10))
	GameConfig = okConfig and type(config) == "table" and config or nil
	UpgradeRules = okRules and type(rules) == "table" and rules or nil
	Supported = GameConfig ~= nil and type(GameConfig.Economy) == "table"
end
restoreIdentity()

if not Supported then
	notify("Ouroboros", "Merge Hackers was not detected, the autofarm did not start.", 10)
	task.delay(10, function()
		Farm.Unload(false)
	end)
	return
end

local Economy = GameConfig.Economy
local TierNames = type(Economy.TierNames) == "table" and Economy.TierNames or {}
local TierIndex = {}
for index, name in TierNames do
	TierIndex[name] = index
end
local BulkQuantities = type(Economy.BulkQuantities) == "table" and Economy.BulkQuantities or { 1, 5, 25, 100 }
local Suffixes = type(GameConfig.NumberSuffixes) == "table" and GameConfig.NumberSuffixes or { "k", "m", "b", "t", "q", "Q", "s", "S", "o", "n", "d" }
local SuffixValue = {}
for index, suffix in Suffixes do
	SuffixValue[suffix] = 1000 ^ index
end
local PotionConfig = type(GameConfig.Potions) == "table" and GameConfig.Potions or {}
local PotionDefs = type(PotionConfig.Definitions) == "table" and PotionConfig.Definitions or {}
local PotionOrder = type(PotionConfig.Order) == "table" and PotionConfig.Order or {}
local QuestConfig = type(GameConfig.Quests) == "table" and GameConfig.Quests or {}
local QuestKinds = type(QuestConfig.Kinds) == "table" and QuestConfig.Kinds or { "Buy", "Merge", "Create", "Collect", "Convert", "Earn", "Upgrade", "Index" }
local ColorStates = {}
for key, color in (type(GameConfig.Colors) == "table" and GameConfig.Colors or {}) do
	if typeof(color) == "Color3" then
		ColorStates[string.lower(tostring(key))] = color
	end
end
local PlotPresence = Shared:FindFirstChild("PlotPresence")
local TouchFunction = (callable(firetouchinterest) and firetouchinterest) or (callable(firetouchtransmitter) and firetouchtransmitter) or nil

local PAD_ORDER = { "CollectButton", "ConvertButton", "MergeButton", "RewardButton", "Upgrade1", "Upgrade2", "Button1", "Button2", "Button3", "Button4" }
local BUY_PADS = { "Button1", "Button2", "Button3", "Button4" }
local BuyPadNames = {}
local PAD_LABELS = {
	CollectButton = "Collect",
	ConvertButton = "Convert",
	MergeButton = "Merge",
	RewardButton = "Reward",
	Upgrade1 = "Convert Speed",
	Upgrade2 = "Buy Tier",
}
for index, name in BUY_PADS do
	local label = "Buy " .. tostring(BulkQuantities[index] or index)
	BuyPadNames[index] = label
	PAD_LABELS[name] = label
end

local GEM_KEYS = { "ConvertSpeed", "DoubleDrop", "GemValue", "GemChance", "StartingCash", "OfflineRewards" }
local GEM_LABELS = {
	ConvertSpeed = "Convert Speed",
	DoubleDrop = "Double Drop",
	GemValue = "Gem Value",
	GemChance = "Gem Chance",
	StartingCash = "Starting Cash",
	OfflineRewards = "Offline Rewards",
}
local GemNames = {}
local GemKeyByName = {}
for _, key in GEM_KEYS do
	table.insert(GemNames, GEM_LABELS[key])
	GemKeyByName[GEM_LABELS[key]] = key
end

local ROMAN = { "I", "II", "III", "IV", "V" }
local PotionNames = {}
local PotionIdByName = {}
local PotionLabels = {}
for _, id in PotionOrder do
	local def = PotionDefs[id]
	local kind = def and def.Kind == "Converter" and "Speed" or (def and tostring(def.Kind) or id)
	local tier = tonumber(string.match(id, "%d+$"))
	local label = kind .. " " .. (tier and ROMAN[tier] or id)
	PotionLabels[id] = label
	table.insert(PotionNames, label)
	PotionIdByName[label] = id
end

local DIFFICULTY_NAMES = { "Easy", "Medium", "Hard" }

local Config = {
	Enabled = true,
	AutoCollectCards = true,
	AutoConvert = true,
	AutoCollectCash = true,
	AutoMerge = true,
	AutoBuy = true,
	BuyPads = { Button1 = true, Button2 = true, Button3 = true, Button4 = true },
	AutoConvertSpeed = true,
	BacklogHigh = 6,
	BacklogLow = 1.5,
	ConverterHeadroom = 4,
	AutoBuyTier = true,
	BuyTierWindow = 300,
	AutoRebirth = true,
	SaveMode = "Smart",
	SaveWindow = 90,
	AutoQuests = true,
	QuestHelper = true,
	RerollKinds = {},
	AutoMilestones = true,
	AutoReward = true,
	AutoOffline = true,
	AutoGemUpgrades = true,
	GemKeys = { ConvertSpeed = true, DoubleDrop = true, GemValue = true, GemChance = true, StartingCash = true, OfflineRewards = true },
	AutoPotions = true,
	AutoBuyPotions = true,
	BuyPotionIds = { Gem3 = true, Gem2 = true, Gem1 = true, Cash3 = true, Cash2 = true, Cash1 = true },
}

local State = {
	Plot = nil,
	PlotAsked = 0,
	Cash = 0,
	Cards = 0,
	Gems = 0,
	Synced = false,
	Pending = 0,
	Rate = 0,
	Producible = 0,
	Rebirth = nil,
	RebirthAt = 0,
	Upgrades = nil,
	Quests = nil,
	Potions = nil,
	PotionsAt = 0,
	Milestones = nil,
	OfflinePending = 0,
	Busy = nil,
}

local Stats = {
	Units = 0,
	Merges = 0,
	Converts = 0,
	Cashouts = 0,
	ConverterUpgrades = 0,
	TierUpgrades = 0,
	Rewards = 0,
	Earned = 0,
	Rebirths = 0,
	Quests = 0,
	Rerolls = 0,
	Milestones = 0,
	GemUpgrades = 0,
	Potions = 0,
	PotionsBought = 0,
	Offline = 0,
	Presses = 0,
	Releases = 0,
}

local Econ = {
	Backlog = 0,
	Converting = false,
	QuestPush = false,
	Saving = false,
	SaveAt = 0,
	TierWanted = false,
	ModeAt = 0,
	Income = 0,
	Production = 0,
	CashRatio = 0,
	GemIncome = 0,
	Payback = nil,
	Plan = nil,
	RebirthTry = 0,
	QuestTrip = 0,
	QuestBackoff = 12,
	MilestoneAt = 0,
	MilestoneBackoff = 8,
	MilestoneGifts = 0,
	GemAt = 0,
	PotionAt = 0,
	PotionBuyAt = 0,
	OfflineAt = 0,
	ResetAt = os.clock(),
	RunStart = nil,
	LastRun = nil,
	WarmupUntil = 0,
}

local Samples = {
	Cash = {},
	Production = {},
	Gems = {},
}

for _, name in PAD_ORDER do
	Pads[name] = { Held = false, LastPress = 0, LastBump = 0, LastRelease = 0, Event = nil }
end
if type(Handover) == "table" and type(Handover.Held) == "table" then
	for name in Handover.Held do
		if Pads[name] then
			Pads[name].Held = true
		end
	end
end

local CARRY_ECON = { "Saving", "SaveAt", "Converting", "ModeAt", "Backlog", "CashRatio", "Payback", "ResetAt", "RunStart", "LastRun", "QuestBackoff" }
Econ.WarmupUntil = os.clock() + 30
if type(Handover) == "table" and type(Handover.Econ) == "table" then
	for _, key in CARRY_ECON do
		if Handover.Econ[key] ~= nil then
			Econ[key] = Handover.Econ[key]
		end
	end
	Econ.WarmupUntil = 0
end
if type(Handover) == "table" and type(Handover.Samples) == "table" then
	for key, list in Handover.Samples do
		if Samples[key] and type(list) == "table" then
			Samples[key] = list
		end
	end
end

function Carry.Snapshot()
	local econ = {}
	for _, key in CARRY_ECON do
		econ[key] = Econ[key]
	end
	return { Econ = econ, Samples = Samples }
end

local function trimZeros(text)
	if string.find(text, ".", 1, true) then
		text = string.gsub(text, "0+$", "")
		text = string.gsub(text, "%.$", "")
	end
	return text
end

local function formatNumber(value)
	value = tonumber(value) or 0
	if value ~= value or value == math.huge or value == -math.huge then
		return "0"
	end
	local sign = value < 0 and "-" or ""
	value = math.abs(value)
	if value < 1000 then
		return sign .. trimZeros(string.format(value < 10 and "%.2f" or "%.1f", value))
	end
	local index = math.min(math.floor(math.log10(value) / 3), #Suffixes)
	local scaled = value / 1000 ^ index
	local pattern = scaled >= 100 and "%.0f" or (scaled >= 10 and "%.1f" or "%.2f")
	return sign .. trimZeros(string.format(pattern, scaled)) .. (Suffixes[index] or "")
end

local function formatMoney(value)
	return "$" .. formatNumber(value)
end

local function formatDuration(seconds)
	if type(seconds) ~= "number" or seconds ~= seconds or seconds == math.huge then
		return "-"
	end
	local total = math.max(0, math.floor(seconds + 0.5))
	if total < 60 then
		return total .. "s"
	elseif total < 3600 then
		return string.format("%dm %02ds", total // 60, total % 60)
	elseif total < 86400 then
		return string.format("%dh %02dm", total // 3600, (total % 3600) // 60)
	end
	return string.format("%dd %02dh", total // 86400, (total % 86400) // 3600)
end

local function parseMoney(text)
	if type(text) ~= "string" then
		return nil
	end
	local digits, suffix = string.match(text, "%$%s*([%d][%d,]*%.?%d*)%s*(%a?)")
	if not digits then
		return nil
	end
	local number = tonumber((string.gsub(digits, ",", "")))
	if not number then
		return nil
	end
	if suffix ~= "" then
		local multiplier = SuffixValue[suffix]
		if not multiplier then
			return nil
		end
		number *= multiplier
	end
	return number
end

local function findPlot()
	local plot = State.Plot
	if plot and plot.Parent then
		return plot
	end
	State.Plot = nil
	if PlotPresence and PlotPresence:IsA("ModuleScript") then
		local ok, presence = pcall(require, PlotPresence)
		if ok and type(presence) == "table" and callable(presence.MyPlot) then
			local okPlot, result = pcall(presence.MyPlot)
			if okPlot and typeof(result) == "Instance" and result.Parent then
				State.Plot = result
			end
		end
		restoreIdentity()
	end
	if not State.Plot and os.clock() - State.PlotAsked > 5 then
		State.PlotAsked = os.clock()
		pcall(function()
			Net.PlotAssigned:FireServer()
		end)
	end
	return State.Plot
end

local function padPart(name)
	local plot = State.Plot
	local buttons = plot and plot:FindFirstChild("Buttons")
	local model = buttons and buttons:FindFirstChild(name)
	local part = model and model:FindFirstChild("TouchPart")
	if part and part:IsA("BasePart") then
		return part
	end
	return nil
end

local function padLabel(name, labelName)
	local part = padPart(name)
	local gui = part and part:FindFirstChild("BillboardGui")
	local label = gui and gui:FindFirstChild(labelName, true)
	if label and label:IsA("TextLabel") then
		return label.Text
	end
	return nil
end

local function padCost(name)
	local text = padLabel(name, "Cost") or padLabel(name, "Amount")
	if not text or string.find(string.upper(text), "REQUIRED", 1, true) then
		return nil
	end
	return parseMoney(text)
end

local function colorState(color)
	local best, bestDistance = nil, math.huge
	for name, reference in ColorStates do
		local distance = math.abs(color.R - reference.R) + math.abs(color.G - reference.G) + math.abs(color.B - reference.B)
		if distance < bestDistance then
			best, bestDistance = name, distance
		end
	end
	if bestDistance <= 0.06 then
		return best
	end
	return nil
end

local function padState(name)
	local part = padPart(name)
	if part then
		local fromColor = colorState(part.Color)
		if fromColor then
			return fromColor
		end
	end
	local info = Pads[name]
	return info and info.Event or nil
end

local function characterRoot()
	local character = LocalPlayer.Character
	if not character then
		return nil, nil
	end
	local root = character:FindFirstChild("HumanoidRootPart")
	local humanoid = character:FindFirstChildOfClass("Humanoid")
	if root and root:IsA("BasePart") and humanoid and humanoid.Health > 0 then
		return root, humanoid
	end
	return nil, nil
end

local function pressPad(name)
	local root = characterRoot()
	local part = padPart(name)
	if not root or not part or not TouchFunction then
		return false
	end
	local info = Pads[name]
	pcall(TouchFunction, root, part, 1)
	task.wait()
	local current = characterRoot()
	pcall(TouchFunction, current or root, part, 0)
	info.Held = true
	info.LastPress = os.clock()
	Stats.Presses += 1
	return true
end

local Stepping = nil

local function stepRelease(names, force)
	local root = characterRoot()
	if not root or #names == 0 then
		return false
	end
	local home = Stepping or root.CFrame
	Stepping = home
	local facing = home - home.Position
	local count = 0
	for _, name in names do
		if not force and not Farm.Running then
			break
		end
		local current, humanoid = characterRoot()
		if not current then
			break
		end
		local info = Pads[name]
		local part = padPart(name)
		if info then
			info.LastRelease = os.clock()
		end
		if part then
			local hip = humanoid and humanoid.HipHeight or 2
			if hip <= 0 then
				hip = 2
			end
			local lift = part.Size.Y / 2 + current.Size.Y / 2 + hip
			current.CFrame = CFrame.new(part.Position + Vector3.new(0, lift, 0)) * facing
			current.AssemblyLinearVelocity = Vector3.zero
			task.wait(0.3)
			count += 1
		end
		if info then
			info.Held = false
			info.LastRelease = os.clock()
		end
	end
	local current = characterRoot()
	if current then
		current.CFrame = home
		current.AssemblyLinearVelocity = Vector3.zero
	end
	Stepping = nil
	Stats.Releases += count
	return count > 0
end

local Vacuumed = setmetatable({}, { __mode = "k" })

local function vacuumCards()
	local plot = State.Plot
	local folder = plot and plot:FindFirstChild("Cards")
	local root = characterRoot()
	if not folder or not root then
		return
	end
	local target = root.CFrame
	for _, card in folder:GetChildren() do
		if not Vacuumed[card] then
			Vacuumed[card] = true
			for _, part in card:GetDescendants() do
				if part:IsA("BasePart") then
					part.Anchored = true
					part.CanCollide = false
				end
			end
			if card:IsA("BasePart") then
				card.Anchored = true
				card.CanCollide = false
			end
		end
		if card:IsA("Model") then
			card:PivotTo(target)
		elseif card:IsA("BasePart") then
			card.CFrame = target
		end
	end
end

local function pushSample(list, value)
	local now = os.clock()
	table.insert(list, { now, value })
	while #list > 0 and now - list[1][1] > 90 do
		table.remove(list, 1)
	end
end

local function rateOver(list, window)
	local now = os.clock()
	local total = 0
	for index = #list, 1, -1 do
		local sample = list[index]
		if now - sample[1] > window then
			break
		end
		total += sample[2]
	end
	local span = math.clamp(now - Econ.ResetAt, 5, window)
	return total / span
end

local function boughtTier()
	local text = padLabel("Button1", "Name")
	local name = text and string.match(text, "^%s*Buy%s+[%d,]+%s+(.-)%s*$")
	return (name and TierIndex[name]) or 1, name
end

local function unitEquivalents()
	local plot = State.Plot
	local buildings = plot and plot:FindFirstChild("Buildings")
	if not buildings then
		return 0
	end
	local total = 0
	for _, building in buildings:GetChildren() do
		local index = TierIndex[building.Name]
		if index then
			total += 3 ^ (index - 1)
		end
	end
	return total
end

local function cashRatio()
	if Econ.CashRatio > 0 then
		return Econ.CashRatio
	end
	local info = State.Rebirth
	local potions = State.Potions
	local mult = type(info) == "table" and tonumber(info.mult) or 1
	local boost = type(potions) == "table" and tonumber(potions.cash) or 1
	return mult * math.max(boost, 1)
end

local function unitPayback()
	local price = padCost("Button1")
	local equivalents = unitEquivalents()
	local production = Econ.Production
	if not price or equivalents <= 0 or production <= 0 then
		return nil
	end
	local perUnit = production * cashRatio() / equivalents * 3 ^ (boughtTier() - 1)
	if perUnit <= 0 then
		return nil
	end
	return price / perUnit
end

local function timeToAfford(cost)
	if not cost then
		return math.huge
	end
	local missing = cost - State.Cash
	if missing <= 0 then
		return 0
	end
	if Econ.Income <= 0 then
		return math.huge
	end
	return missing / Econ.Income
end

local function rebirthPlan()
	local info = State.Rebirth
	if type(info) ~= "table" or info.maxed == true then
		return nil
	end
	local required = tonumber(info.requiredCash)
	if not required then
		return nil
	end
	local remaining = math.max(required - State.Cash, 0)
	local eta = Econ.Income > 0 and remaining / Econ.Income or math.huge
	if remaining <= 0 then
		eta = 0
	end
	local payback = Econ.Payback
	local threshold = Config.SaveWindow
	if Config.SaveMode == "Smart" and payback then
		threshold = math.clamp(payback, 15, 7200)
	end
	return {
		Required = required,
		Remaining = remaining,
		Eta = eta,
		Threshold = threshold,
		Ready = info.ready == true,
		Rebirths = tonumber(info.rebirths) or 0,
		Mult = tonumber(info.mult) or 1,
		NextMult = tonumber(info.nextMult),
	}
end

local function upgradeQuestOpen()
	local quests = State.Quests
	if type(quests) ~= "table" or type(quests.slots) ~= "table" or quests.locked == true then
		return false
	end
	for _, slot in quests.slots do
		if type(slot) == "table" and slot.kind == "Upgrade" then
			local progress, target = tonumber(slot.progress) or 0, tonumber(slot.target) or 0
			if target > 0 and progress < target then
				return true
			end
		end
	end
	return false
end

local function updateEconomy(dt)
	Econ.Production = rateOver(Samples.Production, 60)
	Econ.Income = rateOver(Samples.Cash, 30)
	Econ.GemIncome = rateOver(Samples.Gems, 120)
	local backlog = State.Rate > 0 and State.Producible / State.Rate or 0
	local alpha = 1 - math.exp(-math.max(dt, 0) / 3)
	Econ.Backlog += (backlog - Econ.Backlog) * alpha
	local steadyIncome = rateOver(Samples.Cash, 60)
	if Econ.Production > 0 and steadyIncome > 0 and Econ.Backlog < Config.BacklogHigh and os.clock() - Econ.ResetAt > 30 then
		local ratio = steadyIncome / Econ.Production
		if Econ.CashRatio <= 0 then
			Econ.CashRatio = ratio
		else
			Econ.CashRatio += (ratio - Econ.CashRatio) * math.min(math.max(dt, 0) / 20, 1)
		end
	end
	if Econ.Income <= 0 and State.Rate > 0 and Econ.Production > 0 then
		Econ.Income = math.min(State.Rate, Econ.Production * cashRatio())
	end
	local rawPayback = unitPayback()
	if rawPayback then
		if Econ.Payback then
			Econ.Payback += (rawPayback - Econ.Payback) * (1 - math.exp(-math.max(dt, 0) / 20))
		else
			Econ.Payback = rawPayback
		end
	end

	local plan = rebirthPlan()
	Econ.Plan = plan
	local now = os.clock()
	if Config.Enabled and Config.AutoRebirth and plan and not plan.Ready then
		if Econ.Saving then
			if plan.Eta > plan.Threshold * 2 and now - Econ.SaveAt > 45 then
				Econ.Saving = false
				Econ.SaveAt = now
			end
		elseif now - Econ.ResetAt > 30 and now - Econ.SaveAt > 15 and plan.Eta <= plan.Threshold then
			Econ.Saving = true
			Econ.SaveAt = now
		end
	else
		Econ.Saving = false
	end

	local upgradeState = padState("Upgrade1")
	local upgradeCost = padCost("Upgrade1")
	local canUpgrade = Config.Enabled and Config.AutoConvertSpeed and upgradeState ~= nil and upgradeState ~= "grey" and upgradeCost ~= nil
	if canUpgrade and Econ.Saving and plan and (upgradeCost > plan.Remaining * 0.1 or upgradeCost > math.max(Econ.Income, 1) * 30) then
		canUpgrade = false
	end
	Econ.QuestPush = canUpgrade and not Econ.Saving and Config.QuestHelper and upgradeQuestOpen() and upgradeCost <= math.max(Econ.Income, 1) * 20 or false
	if not canUpgrade then
		Econ.Converting = false
	elseif Econ.Converting then
		local headroomTarget = Econ.Saving and 1.2 or Config.ConverterHeadroom
		local headroom = Econ.Production <= 0 or State.Rate >= Econ.Production * cashRatio() * headroomTarget
		if not Econ.QuestPush and backlog < Config.BacklogLow and headroom and os.clock() - Econ.ModeAt > 4 then
			Econ.Converting = false
			Econ.Backlog = backlog
		end
	elseif Econ.Backlog > Config.BacklogHigh or Econ.QuestPush then
		Econ.Converting = true
		Econ.ModeAt = os.clock()
	end

	local tierState = padState("Upgrade2")
	local tierCost = padCost("Upgrade2")
	Econ.TierWanted = false
	if Config.Enabled and Config.AutoBuyTier and not Econ.Saving and tierCost and (tierState == "green" or tierState == "red" or tierState == "yellow") then
		Econ.TierWanted = tierState == "green" or timeToAfford(tierCost) <= Config.BuyTierWindow
	end
end

local function desiredPads()
	local desired = {}
	if not Config.Enabled then
		return desired
	end
	desired.CollectButton = Config.AutoCollectCash
	desired.ConvertButton = Config.AutoConvert
	desired.MergeButton = Config.AutoMerge
	desired.RewardButton = Config.AutoReward
	local warming = os.clock() < (Econ.WarmupUntil or 0)
	desired.Upgrade1 = Econ.Converting and not warming
	desired.Upgrade2 = Econ.TierWanted and not warming
	local priority = nil
	if Econ.Converting then
		priority = padCost("Upgrade1")
	elseif Econ.TierWanted then
		priority = padCost("Upgrade2")
	end
	local allowBuy = Config.AutoBuy and not Econ.Saving and not warming
	for _, name in BUY_PADS do
		local want = allowBuy and Config.BuyPads[name] == true
		if want and (Econ.Converting or Econ.TierWanted) then
			local price = padCost(name)
			want = Pads[name].Held and priority ~= nil and price ~= nil and price >= priority * 1.25
		end
		desired[name] = want
	end
	return desired
end

local function actuate(desired)
	if State.Busy or not State.Plot or not characterRoot() then
		return
	end
	local now = os.clock()
	local releases = {}
	for _, name in PAD_ORDER do
		local info = Pads[name]
		if desired[name] then
			local stateName = padState(name)
			if (stateName == "green" or stateName == "yellow") and now - info.LastBump > 2.5 and now - info.LastPress > 3 and now - info.LastRelease > 1 then
				pressPad(name)
			end
		elseif info.Held and now - info.LastPress > 0.6 then
			table.insert(releases, name)
		end
	end
	if #releases > 0 then
		State.Busy = "Releasing pads"
		local ok, err = pcall(stepRelease, releases, false)
		State.Busy = nil
		if not ok then
			fail(err)
		end
	end
end

local function modeText()
	if not Config.Enabled then
		return "Paused", "grey"
	end
	if State.Busy then
		return State.Busy, "blue"
	end
	if not State.Plot then
		return "Waiting for plot", "grey"
	end
	if not characterRoot() then
		return "Waiting for character", "grey"
	end
	if os.clock() < (Econ.WarmupUntil or 0) then
		return "Measuring income before spending", "grey"
	end
	if Econ.Converting then
		return Econ.QuestPush and "Finishing upgrade quest" or "Upgrading converter", "blue"
	end
	if Econ.TierWanted then
		return "Saving for buy tier", "orange"
	end
	if Econ.Saving then
		return "Saving for rebirth", "orange"
	end
	if Config.AutoBuy then
		return "Buying units", "green"
	end
	return "Collecting", "green"
end

local function tryRebirth()
	local info = State.Rebirth
	if not Config.Enabled or not Config.AutoRebirth or type(info) ~= "table" or info.ready ~= true or info.maxed == true then
		return
	end
	local now = os.clock()
	if now - Farm.StartedAt < 8 or now - Econ.RebirthTry < 6 or now - State.RebirthAt > 10 then
		return
	end
	Econ.RebirthTry = now
	Net.Rebirth:FireServer(true)
	task.delay(1.5, function()
		if Farm.Running then
			pcall(function()
				Net.Rebirth:FireServer()
			end)
		end
	end)
end

local function onRebirthed(info)
	Stats.Rebirths += 1
	if Econ.RunStart then
		Econ.LastRun = os.clock() - Econ.RunStart
	end
	Econ.RunStart = os.clock()
	table.clear(Samples.Cash)
	table.clear(Samples.Production)
	Econ.ResetAt = os.clock()
	Econ.Saving = false
	Econ.SaveAt = 0
	Econ.Converting = false
	Econ.Backlog = 0
	Econ.CashRatio = 0
	Econ.Payback = nil
	notify("Ouroboros", string.format("Rebirth %d done, multiplier x%s", tonumber(info.rebirths) or 0, formatNumber(tonumber(info.mult) or 1)), 6)
	task.delay(1, function()
		if Farm.Running then
			pcall(function()
				Net.RequestSync:FireServer()
				Net.RebirthUpgrades:FireServer("sync")
			end)
		end
	end)
end

local function nearQuestBoard(root)
	for _, paper in CollectionService:GetTagged("QuestPaper") do
		if paper:IsA("BasePart") and paper:IsDescendantOf(Workspace) and (paper.Position - root.Position).Magnitude <= 30 then
			return true
		end
	end
	return false
end

local function nearHome(root)
	local plot = State.Plot
	local spawnPart = plot and plot:FindFirstChild("Spawn")
	if spawnPart and spawnPart:IsA("BasePart") then
		return (spawnPart.Position - root.Position).Magnitude <= 60
	end
	return true
end

local function waitFor(check, timeout)
	local started = os.clock()
	while Farm.Running and os.clock() - started < timeout do
		local root = characterRoot()
		if root and check(root) then
			return true
		end
		task.wait(0.1)
	end
	return false
end

local function completedQuestIds()
	local quests = State.Quests
	local ids = {}
	if type(quests) ~= "table" or type(quests.slots) ~= "table" then
		return ids
	end
	for key, slot in quests.slots do
		if type(slot) == "table" then
			local progress, target = tonumber(slot.progress), tonumber(slot.target)
			if progress and target and target > 0 and progress >= target then
				ids[tostring(key) .. ":" .. tostring(slot.id)] = true
			end
		end
	end
	return ids
end

local QuestSeen = {}
local RerolledAt = {}

local function pendingQuestWork()
	local quests = State.Quests
	if not Config.Enabled or not Config.AutoQuests or type(quests) ~= "table" or quests.locked == true or type(quests.slots) ~= "table" then
		return nil, nil
	end
	if os.clock() - Econ.QuestTrip < Econ.QuestBackoff then
		return nil, nil
	end
	local claims, rerolls = {}, {}
	local now = os.clock()
	local serverNow = Workspace:GetServerTimeNow()
	local cooldowns = type(quests.rerolls) == "table" and quests.rerolls or {}
	for key, slot in quests.slots do
		local difficulty = tonumber(key)
		if difficulty and type(slot) == "table" and slot.id ~= nil then
			local progress, target = tonumber(slot.progress), tonumber(slot.target)
			local marker = tostring(key) .. ":" .. tostring(slot.id)
			if progress and target and target > 0 and progress >= target then
				QuestSeen[marker] = QuestSeen[marker] or now
				if now - QuestSeen[marker] >= 6 then
					table.insert(claims, { difficulty, slot.id })
				end
			elseif Config.RerollKinds[slot.kind] and (tonumber(cooldowns[tostring(key)]) or 0) <= serverNow then
				table.insert(rerolls, { difficulty, slot.id })
			end
		end
	end
	if #claims == 0 and #rerolls == 0 then
		return nil, nil
	end
	return claims, rerolls
end

local function questTrip(claims, rerolls)
	local root = characterRoot()
	if not root then
		return
	end
	State.Busy = "Claiming quests"
	Econ.QuestTrip = os.clock()
	local before = completedQuestIds()
	local travelled = false
	if not nearQuestBoard(root) then
		travelled = true
		Net.GoQuestPotions:FireServer()
		waitFor(nearQuestBoard, 5)
	end
	local current = characterRoot()
	local claimed = 0
	if Farm.Running and current and nearQuestBoard(current) then
		for _, claim in claims do
			Net.Quests:FireServer("claim", claim[1], claim[2])
			claimed += 1
			task.wait(0.35)
		end
		for _, reroll in rerolls do
			RerolledAt[tostring(reroll[1])] = os.clock()
			Net.Quests:FireServer("reroll", reroll[1], reroll[2])
			Stats.Rerolls += 1
			task.wait(0.35)
		end
		task.wait(0.4)
		Net.Quests:FireServer("sync")
		task.wait(0.6)
	end
	if travelled and Farm.Running then
		Net.GoHome:FireServer()
		if not waitFor(nearHome, 4) then
			task.wait(0.6)
			Net.GoHome:FireServer()
			waitFor(nearHome, 3)
		end
	end
	local after = completedQuestIds()
	local stuck = claimed > 0
	for id in before do
		if not after[id] then
			stuck = false
		end
	end
	if stuck then
		Econ.QuestBackoff = math.min(Econ.QuestBackoff * 2, 300)
	else
		Econ.QuestBackoff = 12
	end
	State.Busy = nil
end

local function giftCount()
	local data = State.Milestones
	local gifts = 0
	if type(data) ~= "table" then
		return 0
	end
	for _, entry in data do
		if type(entry) == "table" and (tonumber(entry.g) or 0) > 0 then
			gifts += 1
		end
	end
	return gifts
end

local function claimMilestones()
	if not Config.Enabled or not Config.AutoMilestones then
		return
	end
	local gifts = giftCount()
	if gifts <= 0 then
		Econ.MilestoneBackoff = 8
		return
	end
	if os.clock() - Econ.MilestoneAt < Econ.MilestoneBackoff then
		return
	end
	if Econ.MilestoneAt > 0 and gifts >= Econ.MilestoneGifts and os.clock() - Econ.MilestoneAt < 30 then
		Econ.MilestoneBackoff = math.min(Econ.MilestoneBackoff * 2, 120)
	end
	Econ.MilestoneAt = os.clock()
	Econ.MilestoneGifts = gifts
	Net.Milestones:FireServer("all")
	task.delay(1, function()
		if Farm.Running then
			pcall(function()
				Net.Milestones:FireServer()
			end)
		end
	end)
end

local GEM_GAIN = {
	GemChance = function(level)
		return 1 / (level + 1)
	end,
	GemValue = function(level)
		return 0.1 / (1 + 0.1 * level)
	end,
	DoubleDrop = function(level)
		return 1.5 * 0.025 / (1 + 0.025 * level)
	end,
	StartingCash = function()
		return 0.02
	end,
	ConvertSpeed = function(level)
		return 0.004 / (1 + 0.1 * level)
	end,
	OfflineRewards = function(level)
		return 0.002 / (1 + 0.05 * level)
	end,
}

local POTION_PRIORITY = { "Gem3", "Gem2", "Gem1", "Cash3", "Cash2", "Cash1", "Converter3", "Converter2", "Converter1" }

local function potionReserve()
	if not Config.AutoBuyPotions then
		return 0
	end
	local total = 0
	for id, enabled in Config.BuyPotionIds do
		local def = PotionDefs[id]
		if enabled and def then
			total += tonumber(def.GemCost) or 0
		end
	end
	return total
end

local function bestGemUpgrade(info)
	local prices = type(UpgradeRules.Prices) == "table" and UpgradeRules.Prices or {}
	local maxima = type(UpgradeRules.Max) == "table" and UpgradeRules.Max or {}
	local levels = type(info.levels) == "table" and info.levels or {}
	local bestKey, bestPrice, bestScore = nil, nil, 0
	for _, key in GEM_KEYS do
		if Config.GemKeys[key] then
			local level = tonumber(levels[key]) or 0
			local cap = tonumber(maxima[key]) or 0
			local list = prices[key]
			local price = type(list) == "table" and tonumber(list[level + 1]) or nil
			local gain = GEM_GAIN[key] and GEM_GAIN[key](level) or 0
			if level < cap and price and price > 0 and gain > 0 then
				local score = gain / price
				if score > bestScore then
					bestKey, bestPrice, bestScore = key, price, score
				end
			end
		end
	end
	return bestKey, bestPrice
end

local function buyGemUpgrade()
	local info = State.Upgrades
	if not Config.Enabled or not Config.AutoGemUpgrades or type(info) ~= "table" or info.unlocked ~= true or not UpgradeRules then
		return
	end
	if os.clock() - Econ.GemAt < 1.2 then
		return
	end
	local gems = State.Synced and State.Gems or (tonumber(info.gems) or 0)
	local bestKey, bestPrice = bestGemUpgrade(info)
	if bestKey and bestPrice and gems - potionReserve() < bestPrice then
		bestKey = nil
	end
	if bestKey then
		Econ.GemAt = os.clock()
		Net.RebirthUpgrades:FireServer("buy", bestKey)
		Stats.GemUpgrades += 1
		task.delay(0.6, function()
			if Farm.Running then
				pcall(function()
					Net.RebirthUpgrades:FireServer("sync")
				end)
			end
		end)
	end
end

local function handlePotions()
	local potions = State.Potions
	if not Config.Enabled or type(potions) ~= "table" then
		return
	end
	local now = os.clock()
	if State.PotionsAt < Econ.PotionAt and now - Econ.PotionAt < 10 then
		return
	end
	local elapsed = now - State.PotionsAt
	local inventory = type(potions.inventory) == "table" and potions.inventory or {}
	local active = type(potions.active) == "table" and potions.active or {}
	if Config.AutoPotions and now - Econ.PotionAt > 2 then
		for _, id in PotionOrder do
			local count = tonumber(inventory[id]) or 0
			local remaining = (tonumber(active[id]) or 0) - elapsed
			if count > 0 and remaining <= 0 then
				Econ.PotionAt = now
				Net.Potions:FireServer("use", id)
				Stats.Potions += 1
				task.delay(0.5, function()
					if Farm.Running then
						pcall(function()
							Net.Potions:FireServer("sync")
						end)
					end
				end)
				return
			end
		end
	end
	if Config.AutoBuyPotions and now - Econ.PotionBuyAt > 3 then
		for _, id in POTION_PRIORITY do
			local def = PotionDefs[id]
			local cost = def and tonumber(def.GemCost)
			local count = tonumber(inventory[id]) or 0
			local remaining = (tonumber(active[id]) or 0) - elapsed
			if Config.BuyPotionIds[id] and cost and count <= 0 and remaining <= 5 and State.Gems >= cost then
				Econ.PotionBuyAt = now
				Econ.PotionAt = now
				Net.Potions:FireServer("buy", id)
				Stats.PotionsBought += 1
				task.delay(0.5, function()
					if Farm.Running then
						pcall(function()
							Net.Potions:FireServer("sync")
						end)
					end
				end)
				return
			end
		end
	end
end

local function claimOffline()
	if not Config.Enabled or not Config.AutoOffline or State.OfflinePending <= 0 then
		return
	end
	if os.clock() - Econ.OfflineAt < 5 then
		return
	end
	Econ.OfflineAt = os.clock()
	Stats.Offline += State.OfflinePending
	State.OfflinePending = 0
	Net.OfflineRewards:FireServer("claim")
end

local SyncPlan = {
	{ Every = 5, Last = 0, Remote = "Rebirth", Args = {} },
	{ Every = 15, Last = 0, Remote = "RequestSync", Args = {} },
	{ Every = 12, Last = 0, Remote = "RebirthUpgrades", Args = { "sync" } },
	{ Every = 20, Last = 0, Remote = "Quests", Args = { "sync" } },
	{ Every = 20, Last = 0, Remote = "Potions", Args = { "sync" } },
	{ Every = 25, Last = 0, Remote = "Milestones", Args = {} },
}

local function runSyncs()
	local now = os.clock()
	for _, entry in SyncPlan do
		if now - entry.Last >= entry.Every then
			entry.Last = now
			pcall(function()
				Net[entry.Remote]:FireServer(table.unpack(entry.Args))
			end)
		end
	end
end

local function countAction(name)
	if name == "MergeButton" then
		Stats.Merges += 1
	elseif name == "ConvertButton" then
		Stats.Converts += 1
	elseif name == "CollectButton" then
		Stats.Cashouts += 1
	elseif name == "Upgrade1" then
		Stats.ConverterUpgrades += 1
	elseif name == "Upgrade2" then
		Stats.TierUpgrades += 1
		notify("Ouroboros", "Buy tier upgraded", 5)
	elseif name == "RewardButton" then
		Stats.Rewards += 1
	else
		for index, pad in BUY_PADS do
			if pad == name then
				Stats.Units += tonumber(BulkQuantities[index]) or 1
				break
			end
		end
	end
end

local function ownPad(instance)
	local plot = State.Plot
	if typeof(instance) ~= "Instance" or not plot then
		return nil
	end
	local model = instance
	if instance:IsA("BasePart") then
		model = instance.Parent
	end
	if model and model.Parent and model.Parent.Name == "Buttons" and model.Parent.Parent == plot then
		return model.Name
	end
	return nil
end

connect(Net.PlotAssigned.OnClientEvent, function(plot)
	if typeof(plot) == "Instance" and plot ~= State.Plot then
		State.Plot = plot
		for _, info in Pads do
			info.Event = nil
		end
	end
end)

connect(Net.CurrencyUpdate.OnClientEvent, function(cards, cash, _, gems)
	State.Cards = tonumber(cards) or State.Cards
	State.Cash = tonumber(cash) or State.Cash
	local newGems = tonumber(gems)
	if newGems then
		if State.Synced and newGems > State.Gems then
			pushSample(Samples.Gems, newGems - State.Gems)
		end
		State.Gems = newGems
	end
	State.Synced = true
end)

connect(Net.ConverterUpdate.OnClientEvent, function(_, pending, rate, producible)
	State.Pending = tonumber(pending) or 0
	State.Rate = tonumber(rate) or State.Rate
	State.Producible = tonumber(producible) or 0
end)

connect(Net.ButtonState.OnClientEvent, function(model, stateName)
	local name = ownPad(model)
	if name and Pads[name] and type(stateName) == "string" then
		Pads[name].Event = string.lower(stateName)
	end
end)

connect(Net.Fx.OnClientEvent, function(kind, data)
	if kind == "bump" then
		local name = ownPad(data)
		local info = name and Pads[name]
		if info then
			local now = os.clock()
			info.LastBump = now
			if not info.Held and now - info.LastRelease > 1.5 then
				info.Held = true
			end
			countAction(name)
		end
	elseif kind == "cashout" and type(data) == "table" then
		local amount = tonumber(data.amount) or 0
		if amount > 0 then
			pushSample(Samples.Cash, amount)
			Stats.Earned += amount
		end
	end
end)

connect(Net.CardSpawn.OnClientEvent, function(list)
	if type(list) ~= "table" then
		return
	end
	local plot = State.Plot
	local total = 0
	for _, entry in list do
		if type(entry) == "table" and entry.gem ~= true then
			local building = entry.building
			if not plot or typeof(building) ~= "Instance" or building:IsDescendantOf(plot) then
				total += tonumber(entry.value) or 0
			end
		end
	end
	if total > 0 then
		pushSample(Samples.Production, total)
	end
end)

connect(Net.Rebirth.OnClientEvent, function(info)
	if type(info) ~= "table" then
		return
	end
	local previous = State.Rebirth
	State.Rebirth = info
	State.RebirthAt = os.clock()
	if type(previous) == "table" and tonumber(info.rebirths) and tonumber(previous.rebirths) and tonumber(info.rebirths) > tonumber(previous.rebirths) then
		onRebirthed(info)
	end
end)

connect(Net.RebirthUpgrades.OnClientEvent, function(info)
	if type(info) == "table" and type(info.levels) == "table" then
		State.Upgrades = info
	end
end)

connect(Net.Quests.OnClientEvent, function(info)
	if type(info) == "table" and type(info.slots) == "table" then
		local previous = State.Quests
		if type(previous) == "table" and type(previous.slots) == "table" then
			for key, slot in info.slots do
				local old = previous.slots[key]
				if type(slot) == "table" and type(old) == "table" and old.id ~= nil and slot.id ~= old.id then
					if os.clock() - (RerolledAt[tostring(key)] or 0) > 10 then
						Stats.Quests += 1
					end
				end
			end
		end
		State.Quests = info
	end
end)

connect(Net.Potions.OnClientEvent, function(info)
	if type(info) == "table" and type(info.active) == "table" then
		State.Potions = info
		State.PotionsAt = os.clock()
	end
end)

connect(Net.Milestones.OnClientEvent, function(info)
	if type(info) == "table" then
		local before = giftCount()
		State.Milestones = info
		local after = giftCount()
		if after < before then
			Stats.Milestones += before - after
		end
	end
end)

connect(Net.OfflineRewards.OnClientEvent, function(kind, amount)
	if kind == "pending" then
		State.OfflinePending = tonumber(amount) or 0
	elseif kind == "claimed" then
		State.OfflinePending = 0
	end
end)

local vacuumClock = 0
connect(RunService.Heartbeat, function(dt)
	vacuumClock += dt
	if vacuumClock < 0.1 then
		return
	end
	vacuumClock = 0
	if Farm.Running and Config.Enabled and Config.AutoCollectCards then
		vacuumCards()
	end
end)

table.insert(Farm.Cleanup, function(handover)
	if handover then
		if Stepping then
			local root = characterRoot()
			if root then
				root.CFrame = Stepping
				root.AssemblyLinearVelocity = Vector3.zero
			end
			Stepping = nil
		end
		return
	end
	local held = {}
	for _, name in PAD_ORDER do
		if Pads[name].Held then
			table.insert(held, name)
		end
	end
	task.spawn(function()
		if Stepping then
			local root = characterRoot()
			if root then
				root.CFrame = Stepping
				root.AssemblyLinearVelocity = Vector3.zero
			end
			Stepping = nil
		end
		if #held > 0 then
			pcall(stepRelease, held, true)
		end
	end)
end)

pcall(function()
	Net.PlotAssigned:FireServer()
	Net.OfflineRewards:FireServer()
end)

spawnLoop(function()
	local last = os.clock()
	while Farm.Running do
		task.wait(0.25)
		if not Farm.Running then
			break
		end
		local now = os.clock()
		local dt = now - last
		last = now
		local ok, err = pcall(function()
			findPlot()
			updateEconomy(dt)
			if Config.Enabled and not State.Busy then
				tryRebirth()
				local claims, rerolls = pendingQuestWork()
				if claims then
					questTrip(claims, rerolls)
				end
			end
			actuate(desiredPads())
		end)
		if not ok then
			State.Busy = nil
			fail(err)
		end
	end
end)

spawnLoop(function()
	while Farm.Running do
		local ok, err = pcall(function()
			runSyncs()
			claimOffline()
			claimMilestones()
			buyGemUpgrade()
			handlePotions()
		end)
		if not ok then
			fail(err)
		end
		task.wait(1)
	end
end)

local function setupInterface()
	local GAME_NAME = "Merge Hackers"
	pcall(function()
		GAME_NAME = MarketplaceService:GetProductInfo(game.PlaceId).Name
	end)
	restoreIdentity()
	local SCRIPT_VERSION = "v1.0"
	local Toggles = Library.Toggles
	local Options = Library.Options
	local function track(fn)
		table.insert(Farm.Cleanup, fn)
	end
	local function copyText(text, message)
		local writer = callable(setclipboard) and setclipboard or (callable(toclipboard) and toclipboard or nil)
		if not writer then
			Library:Notify("Clipboard is unavailable")
			return
		end
		local ok = pcall(writer, text)
		if ok then
			Library:Notify(message)
		else
			Library:Notify("Failed to copy")
		end
	end
	local Window = Library:CreateWindow({
		Title = "Ouroboros",
		Font = Enum.Font.BuilderSans,
		Footer = {
			GAME_NAME,
			"|",
			SCRIPT_VERSION,
		},
		Icon = 78539693571783,
		NotifySide = "Right",
		ShowCustomCursor = false,
		CornerRadius = 0,
		SidebarCompacted = true,
		TabSwipeFrom = "bottom",
		Animations = { TabSwitch = true },
	})
	Window:SetGlow(false)
	local Tabs = {
		Info = Window:AddTab("Info", "info"),
		Main = Window:AddTab("Main", "gamepad-2"),
		Economy = Window:AddTab("Economy", "coins"),
		Extras = Window:AddTab("Extras", "gift"),
		Settings = Window:AddTab("Settings", "settings"),
	}
	local function escapeText(text)
		return (tostring(text):gsub("&", "&amp;"):gsub("<", "&lt;"):gsub(">", "&gt;"):gsub('"', "&quot;"):gsub("'", "&apos;"))
	end
	local function colored(text, color)
		return string.format('<font color="%s">%s</font>', color, escapeText(text))
	end
	local function field(key, value, color)
		return string.format("<b>%s</b> %s %s", key, colored("-", "#5a6070"), colored(value, color))
	end
	local GREEN = "#7fd47f"
	local BLUE = "#6ec1ff"
	local ORANGE = "#e8a34d"
	local GREY = "#8b93a3"
	local RED = "#ff6b6b"
	local ModeColors = { green = GREEN, blue = BLUE, orange = ORANGE, grey = GREY }
	local function uiLoop(fn)
		spawnLoop(function()
			while Farm.Running and not Library.Unloaded do
				task.wait(1)
				if not Farm.Running or Library.Unloaded then
					break
				end
				restoreIdentity()
				local ok, err = pcall(fn)
				if not ok then
					fail(err)
				end
			end
		end)
	end
	local function setupInfoTab()
		local executorName = "Unknown"
		pcall(function()
			if callable(identifyexecutor) then
				local name, version = identifyexecutor()
				if type(name) == "string" and name ~= "" then
					executorName = type(version) == "string" and version ~= "" and (name .. " " .. version) or name
				end
			end
		end)
		local sessionStart = os.clock()
		local function sessionElapsed()
			return formatDuration(os.clock() - sessionStart)
		end
		local UserGroup = Tabs.Info:AddLeftGroupbox("User", "circle-user")
		UserGroup:AddPlayerInfo("InfoUserCard", {
			Player = LocalPlayer,
			Title = "User",
			HeaderIcon = "user",
			Collapsible = false,
		})
		UserGroup:AddLabel(field("User", LocalPlayer.DisplayName .. " @" .. LocalPlayer.Name, GREEN), true)
		UserGroup:AddLabel(field("UserId", tostring(LocalPlayer.UserId), BLUE), true)
		UserGroup:AddLabel(field("Executor", executorName, GREEN), true)
		UserGroup:AddDivider()
		local SessionLabel = UserGroup:AddLabel(field("Session", sessionElapsed(), ORANGE), true)
		UserGroup:AddDivider()
		UserGroup:AddButton({
			Text = "Copy Username",
			Func = function()
				copyText(LocalPlayer.Name, "Copied username")
			end,
		})
		UserGroup:AddButton({
			Text = "Copy Profile Link",
			Func = function()
				copyText("https://www.roblox.com/users/" .. tostring(LocalPlayer.UserId) .. "/profile", "Copied profile link")
			end,
		})
		local SessionGroup = Tabs.Info:AddRightGroupbox("Session", "signal")
		SessionGroup:AddLabel(field("Game", GAME_NAME, BLUE), true)
		local PlayersLabel = SessionGroup:AddLabel(field("Players", "0/0", GREEN), true)
		local jobId = tostring(game.JobId)
		local shortJobId = #jobId > 18 and (string.sub(jobId, 1, 18) .. "...") or jobId
		SessionGroup:AddLabel(field("Job", shortJobId, GREY), true)
		local PingLabel = SessionGroup:AddLabel(field("Ping", "0 ms", ORANGE), true)
		SessionGroup:AddDivider()
		SessionGroup:AddButton({
			Text = "Rejoin Place",
			Func = function()
				queueSelf()
				TeleportService:Teleport(game.PlaceId, LocalPlayer)
			end,
		})
		SessionGroup:AddButton({
			Text = "Copy Job ID",
			Func = function()
				copyText(jobId, "Copied Job ID")
			end,
		})
		uiLoop(function()
			local ok, ping = pcall(function()
				return math.floor((game:GetService("Stats") :: any).Network.ServerStatsItem["Data Ping"]:GetValue())
			end)
			restoreIdentity()
			setLabel(SessionLabel, field("Session", sessionElapsed(), ORANGE))
			setLabel(PlayersLabel, field("Players", #Players:GetPlayers() .. "/" .. tostring(Players.MaxPlayers), GREEN))
			setLabel(PingLabel, field("Ping", ok and (tostring(ping) .. " ms") or "n/a", ORANGE))
		end)
	end
	setupInfoTab()
	local function setupMainTab()
		local FarmGroup = Tabs.Main:AddLeftGroupbox("Autofarm", "zap")
		FarmGroup:AddToggle("FarmEnabled", {
			Text = "Enabled",
			Default = Config.Enabled,
			Tooltip = "Master switch. Turning it off lets go of every pad the farm is holding.",
			Callback = function(value)
				Config.Enabled = value
			end,
		})
		FarmGroup:AddToggle("FarmVacuum", {
			Text = "Vacuum Cards",
			Default = Config.AutoCollectCards,
			Tooltip = "Pulls every card on your plot to you so the game collects it at once and drops never stall at the 100 card cap.",
			Callback = function(value)
				Config.AutoCollectCards = value
			end,
		})
		FarmGroup:AddToggle("FarmConvert", {
			Text = "Auto Convert",
			Default = Config.AutoConvert,
			Tooltip = "Keeps the Convert pad pressed so collected cards go straight into the converter.",
			Callback = function(value)
				Config.AutoConvert = value
			end,
		})
		FarmGroup:AddToggle("FarmCollectCash", {
			Text = "Auto Collect Cash",
			Default = Config.AutoCollectCash,
			Tooltip = "Keeps the Collect pad pressed so converted cash is banked every tick.",
			Callback = function(value)
				Config.AutoCollectCash = value
			end,
		})
		FarmGroup:AddToggle("FarmMerge", {
			Text = "Auto Merge",
			Default = Config.AutoMerge,
			Tooltip = "Keeps the Merge pad pressed so every 3 of a kind merges straight away.",
			Callback = function(value)
				Config.AutoMerge = value
			end,
		})
		FarmGroup:AddToggle("FarmBuy", {
			Text = "Auto Buy Units",
			Default = Config.AutoBuy,
			Tooltip = "Keeps the buy pads picked in Economy pressed whenever the plan says to spend.",
			Callback = function(value)
				Config.AutoBuy = value
			end,
		})
		local StatusGroup = Tabs.Main:AddLeftGroupbox("Status", "activity")
		local StateLabel = StatusGroup:AddLabel(field("State", "starting", GREY), true)
		local CashLabel = StatusGroup:AddLabel(field("Cash", "$0", GREEN), true)
		local CardsLabel = StatusGroup:AddLabel(field("Cards", "0", BLUE), true)
		local IncomeLabel = StatusGroup:AddLabel(field("Income", "$0/s", GREEN), true)
		local OutputLabel = StatusGroup:AddLabel(field("Card output", "0/s", BLUE), true)
		local ConverterLabel = StatusGroup:AddLabel(field("Converter", "-", ORANGE), true)
		local BoostLabel = StatusGroup:AddLabel(field("Boosts", "-", ORANGE), true)
		local GemIncomeLabel = StatusGroup:AddLabel(field("Gem income", "-", BLUE), true)
		local UnitLabel = StatusGroup:AddLabel(field("Next unit", "-", GREY), true)
		local HeldLabel = StatusGroup:AddLabel(field("Holding", "none", GREY), true)
		local ErrorLabel = StatusGroup:AddLabel("", true)
		local RebirthGroup = Tabs.Main:AddRightGroupbox("Rebirth", "refresh-cw")
		local RebirthCountLabel = RebirthGroup:AddLabel(field("Rebirths", "-", GREEN), true)
		local RebirthNextLabel = RebirthGroup:AddLabel(field("Next", "-", BLUE), true)
		local RebirthEtaLabel = RebirthGroup:AddLabel(field("ETA", "-", ORANGE), true)
		local RebirthPlanLabel = RebirthGroup:AddLabel(field("Plan", "-", GREY), true)
		local RunLabel = RebirthGroup:AddLabel(field("Run time", "-", GREY), true)
		local SessionGroup = Tabs.Main:AddRightGroupbox("This Session", "bar-chart-3")
		local UnitsLabel = SessionGroup:AddLabel(field("Units bought", "0", GREEN), true)
		local MergesLabel = SessionGroup:AddLabel(field("Merges", "0", GREEN), true)
		local ConverterUpsLabel = SessionGroup:AddLabel(field("Converter levels", "0", BLUE), true)
		local TierUpsLabel = SessionGroup:AddLabel(field("Buy tier ups", "0", BLUE), true)
		local EarnedLabel = SessionGroup:AddLabel(field("Cash banked", "$0", GREEN), true)
		local RebirthsLabel = SessionGroup:AddLabel(field("Rebirths", "0", ORANGE), true)
		local QuestsLabel = SessionGroup:AddLabel(field("Quests claimed", "0", ORANGE), true)
		local ExtrasLabel = SessionGroup:AddLabel(field("Gifts / gem ups / potions", "0 / 0 / 0", GREY), true)
		local function heldText()
			local names = {}
			for _, name in PAD_ORDER do
				if Pads[name].Held then
					table.insert(names, PAD_LABELS[name])
				end
			end
			return #names > 0 and table.concat(names, ", ") or "none"
		end
		uiLoop(function()
			local mode, tone = modeText()
			local plan = Econ.Plan
			local unitPrice = padCost("Button1")
			local _, tierName = boughtTier()
			local paybackText = Econ.Payback and (", pays back in " .. formatDuration(Econ.Payback)) or ""
			setLabel(StateLabel, field("State", mode, ModeColors[tone] or GREY))
			setLabel(CashLabel, field("Cash", formatMoney(State.Cash), GREEN))
			setLabel(CardsLabel, field("Cards", formatNumber(State.Cards) .. "   Gems " .. formatNumber(State.Gems), BLUE))
			setLabel(IncomeLabel, field("Income", formatMoney(Econ.Income) .. "/s", GREEN))
			setLabel(OutputLabel, field("Card output", formatNumber(Econ.Production) .. "/s", BLUE))
			setLabel(ConverterLabel, field("Converter", formatMoney(State.Rate) .. "/s, backlog " .. string.format("%.1fs", Econ.Backlog), ORANGE))
			local rebirthInfo = State.Rebirth
			local potionInfo = State.Potions
			local rebirthMult = type(rebirthInfo) == "table" and tonumber(rebirthInfo.mult) or 1
			local cashBoost = type(potionInfo) == "table" and tonumber(potionInfo.cash) or 1
			local gemBoost = type(potionInfo) == "table" and tonumber(potionInfo.gems) or 1
			setLabel(BoostLabel, field("Boosts", string.format("rebirth x%s, cash potions x%s, gem potions x%s", formatNumber(rebirthMult), formatNumber(cashBoost), formatNumber(gemBoost)), ORANGE))
			setLabel(GemIncomeLabel, field("Gem income", formatNumber(Econ.GemIncome * 3600) .. "/h", BLUE))
			setLabel(UnitLabel, field("Next unit", unitPrice and (formatMoney(unitPrice) .. " " .. tostring(tierName or "") .. paybackText) or "-", GREY))
			setLabel(HeldLabel, field("Holding", heldText(), GREY))
			local problem = Farm.LastError and os.clock() - (Farm.ErrorAt or 0) < 60 and Farm.LastError or nil
			setLabel(ErrorLabel, problem and colored("Last error: " .. string.sub(problem, 1, 140), RED) or "")
			local info = State.Rebirth
			if type(info) == "table" then
				setLabel(RebirthCountLabel, field("Rebirths", tostring(tonumber(info.rebirths) or 0) .. "   x" .. formatNumber(tonumber(info.mult) or 1), GREEN))
			end
			if plan then
				local progress = plan.Required > 0 and math.clamp(State.Cash / plan.Required, 0, 1) or 0
				local nextMult = plan.NextMult and (" for x" .. formatNumber(plan.NextMult)) or ""
				setLabel(RebirthNextLabel, field("Next", formatMoney(plan.Required) .. nextMult .. string.format(" (%d%%)", math.floor(progress * 100)), BLUE))
				setLabel(RebirthEtaLabel, field("ETA", plan.Ready and "ready now" or formatDuration(plan.Eta), ORANGE))
				local planText
				if not Config.AutoRebirth then
					planText = "auto rebirth is off"
				elseif plan.Ready then
					planText = "rebirthing"
				elseif Econ.Saving then
					planText = "saving, buying stopped"
				else
					planText = "buying until the rebirth is " .. formatDuration(plan.Threshold) .. " away"
				end
				setLabel(RebirthPlanLabel, field("Plan", planText, GREY))
			elseif type(info) == "table" and info.maxed == true then
				setLabel(RebirthNextLabel, field("Next", "max rebirth reached", BLUE))
				setLabel(RebirthEtaLabel, field("ETA", "-", ORANGE))
				setLabel(RebirthPlanLabel, field("Plan", "buying", GREY))
			end
			local runText = Econ.RunStart and ("this run " .. formatDuration(os.clock() - Econ.RunStart)) or "this run started before the script"
			if Econ.LastRun then
				runText ..= ", last run " .. formatDuration(Econ.LastRun)
			end
			setLabel(RunLabel, field("Run time", runText, GREY))
			setLabel(UnitsLabel, field("Units bought", formatNumber(Stats.Units), GREEN))
			setLabel(MergesLabel, field("Merges", formatNumber(Stats.Merges), GREEN))
			setLabel(ConverterUpsLabel, field("Converter levels", formatNumber(Stats.ConverterUpgrades), BLUE))
			setLabel(TierUpsLabel, field("Buy tier ups", formatNumber(Stats.TierUpgrades), BLUE))
			setLabel(EarnedLabel, field("Cash banked", formatMoney(Stats.Earned), GREEN))
			setLabel(RebirthsLabel, field("Rebirths", tostring(Stats.Rebirths), ORANGE))
			setLabel(QuestsLabel, field("Quests claimed", tostring(Stats.Quests), ORANGE))
			setLabel(ExtrasLabel, field("Gifts / gem ups / potions", string.format("%d / %d / %d", Stats.Milestones, Stats.GemUpgrades, Stats.Potions + Stats.PotionsBought), GREY))
		end)
	end
	setupMainTab()
	local function setupEconomyTab()
		local SpendGroup = Tabs.Economy:AddLeftGroupbox("Spending", "coins")
		SpendGroup:AddDropdown("BuyPads", {
			Text = "Buy pads",
			Values = BuyPadNames,
			Default = BuyPadNames,
			Multi = true,
			AllowNull = true,
			Tooltip = "Pads kept pressed while buying. More pads spend faster; each one buys as soon as it is affordable.",
			Callback = function(value)
				for index, name in BUY_PADS do
					Config.BuyPads[name] = value[BuyPadNames[index]] == true
				end
			end,
		})
		SpendGroup:AddDivider()
		SpendGroup:AddToggle("AutoConvertSpeed", {
			Text = "Auto Convert Speed",
			Default = Config.AutoConvertSpeed,
			Tooltip = "When cards pile up faster than the converter turns them into cash, buying stops and the converter is upgraded until it keeps up.",
			Callback = function(value)
				Config.AutoConvertSpeed = value
			end,
		})
		SpendGroup:AddSlider("BacklogHigh", {
			Text = "Upgrade when backlog over",
			Default = Config.BacklogHigh,
			Min = 2,
			Max = 30,
			Rounding = 0,
			Suffix = "s",
			Tooltip = "Seconds of cards waiting in the converter before it gets upgraded.",
			Callback = function(value)
				Config.BacklogHigh = value
			end,
		})
		SpendGroup:AddSlider("BacklogLow", {
			Text = "Back to buying under",
			Default = Config.BacklogLow,
			Min = 0.5,
			Max = 10,
			Rounding = 1,
			Suffix = "s",
			Callback = function(value)
				Config.BacklogLow = value
			end,
		})
		SpendGroup:AddSlider("ConverterHeadroom", {
			Text = "Converter headroom",
			Default = Config.ConverterHeadroom,
			Min = 1,
			Max = 4,
			Rounding = 1,
			Suffix = "x",
			Tooltip = "Once upgrading, keeps going until the converter handles this many times your card output, so it pauses buying less often.",
			Callback = function(value)
				Config.ConverterHeadroom = value
			end,
		})
		SpendGroup:AddDivider()
		SpendGroup:AddToggle("AutoBuyTier", {
			Text = "Auto Buy Tier",
			Default = Config.AutoBuyTier,
			Tooltip = "Once the Buy Tier pad unlocks (rebirth 1 and enough units bought), saves up for it and buys it.",
			Callback = function(value)
				Config.AutoBuyTier = value
			end,
		})
		SpendGroup:AddSlider("BuyTierWindow", {
			Text = "Save for buy tier within",
			Default = Config.BuyTierWindow,
			Min = 30,
			Max = 900,
			Rounding = 0,
			Suffix = "s",
			Tooltip = "Buying pauses to save for the next buy tier once it is this close at the current income.",
			Callback = function(value)
				Config.BuyTierWindow = value
			end,
		})
		local RebirthGroup = Tabs.Economy:AddRightGroupbox("Rebirth", "refresh-cw")
		RebirthGroup:AddToggle("AutoRebirth", {
			Text = "Auto Rebirth",
			Default = Config.AutoRebirth,
			Tooltip = "Rebirths as soon as the server says you can. Rebirth needs the cash in hand, so buying stops near the goal.",
			Callback = function(value)
				Config.AutoRebirth = value
			end,
		})
		RebirthGroup:AddDropdown("SaveMode", {
			Text = "Start saving",
			Values = { "Smart", "Fixed window" },
			Default = Config.SaveMode,
			Multi = false,
			Callback = function(value)
				if type(value) == "string" then
					Config.SaveMode = value
				end
			end,
		})
		RebirthGroup:AddSlider("SaveWindow", {
			Text = "Fixed window",
			Default = Config.SaveWindow,
			Min = 15,
			Max = 900,
			Rounding = 0,
			Suffix = "s",
			Callback = function(value)
				Config.SaveWindow = value
			end,
		})
		RebirthGroup:AddLabel("Smart keeps buying while a new unit would earn back its price before the rebirth, then saves. Fixed window saves once the rebirth is that many seconds away.", true)
	end
	setupEconomyTab()
	local function setupExtrasTab()
		local QuestGroup = Tabs.Extras:AddLeftGroupbox("Quests", "scroll")
		QuestGroup:AddToggle("AutoQuests", {
			Text = "Auto Claim Quests",
			Default = Config.AutoQuests,
			Tooltip = "Uses the game's own quest board teleport, claims every finished quest and teleports back home.",
			Callback = function(value)
				Config.AutoQuests = value
			end,
		})
		QuestGroup:AddToggle("QuestHelper", {
			Text = "Finish Upgrade Quests",
			Default = Config.QuestHelper,
			Tooltip = "Upgrade quests only move when the converter levels up, so the farm buys the few cheap converter levels they need instead of leaving them stuck.",
			Callback = function(value)
				Config.QuestHelper = value
			end,
		})
		QuestGroup:AddDropdown("RerollKinds", {
			Text = "Reroll these quests",
			Values = QuestKinds,
			Default = {},
			Multi = true,
			AllowNull = true,
			Tooltip = "Quests of these types are rerolled whenever their reroll is off cooldown.",
			Callback = function(value)
				local kinds = {}
				for kind, enabled in value do
					if enabled then
						kinds[kind] = true
					end
				end
				Config.RerollKinds = kinds
			end,
		})
		local QuestLabels = {}
		for index = 1, 3 do
			QuestLabels[index] = QuestGroup:AddLabel(field(DIFFICULTY_NAMES[index], "-", GREY), true)
		end
		local RewardGroup = Tabs.Extras:AddLeftGroupbox("Rewards", "gift")
		RewardGroup:AddToggle("AutoMilestones", {
			Text = "Auto Milestone Gifts",
			Default = Config.AutoMilestones,
			Tooltip = "Redeems every index milestone gift as soon as one is waiting.",
			Callback = function(value)
				Config.AutoMilestones = value
			end,
		})
		RewardGroup:AddToggle("AutoReward", {
			Text = "Auto Group Reward",
			Default = Config.AutoReward,
			Tooltip = "Presses the reward pad whenever the group payout is ready. The payout needs group membership.",
			Callback = function(value)
				Config.AutoReward = value
			end,
		})
		RewardGroup:AddToggle("AutoOffline", {
			Text = "Auto Offline Rewards",
			Default = Config.AutoOffline,
			Callback = function(value)
				Config.AutoOffline = value
			end,
		})
		local GemGroup = Tabs.Extras:AddRightGroupbox("Gem Upgrades", "gem")
		local defaultGems = {}
		for _, key in GEM_KEYS do
			if Config.GemKeys[key] then
				table.insert(defaultGems, GEM_LABELS[key])
			end
		end
		GemGroup:AddToggle("AutoGemUpgrades", {
			Text = "Auto Gem Upgrades",
			Default = Config.AutoGemUpgrades,
			Tooltip = "Unlocks after the first rebirth. Buys the picked upgrade with the best value per gem, after keeping enough gems to renew the potions.",
			Callback = function(value)
				Config.AutoGemUpgrades = value
			end,
		})
		GemGroup:AddDropdown("GemKeys", {
			Text = "Upgrades",
			Values = GemNames,
			Default = defaultGems,
			Multi = true,
			AllowNull = true,
			Callback = function(value)
				local keys = {}
				for name, enabled in value do
					local key = GemKeyByName[name]
					if key and enabled then
						keys[key] = true
					end
				end
				Config.GemKeys = keys
			end,
		})
		local GemLabel = GemGroup:AddLabel("-", true)
		local PotionGroup = Tabs.Extras:AddRightGroupbox("Potions", "flask-conical")
		PotionGroup:AddToggle("AutoPotions", {
			Text = "Auto Use Potions",
			Default = Config.AutoPotions,
			Tooltip = "Drinks any potion you own as soon as the same potion is not already running.",
			Callback = function(value)
				Config.AutoPotions = value
			end,
		})
		PotionGroup:AddToggle("AutoBuyPotions", {
			Text = "Buy Potions With Gems",
			Default = Config.AutoBuyPotions,
			Tooltip = "Keeps the picked potions running by rebuying each one as it runs out. Cash and gem potions stack, all six together give x12.5 cash and x8.75 gems for about 6k gems an hour.",
			Callback = function(value)
				Config.AutoBuyPotions = value
			end,
		})
		local defaultPotions = {}
		for _, id in PotionOrder do
			if Config.BuyPotionIds[id] and PotionLabels[id] then
				table.insert(defaultPotions, PotionLabels[id])
			end
		end
		PotionGroup:AddDropdown("BuyPotionIds", {
			Text = "Potions to buy",
			Values = PotionNames,
			Default = defaultPotions,
			Multi = true,
			AllowNull = true,
			Callback = function(value)
				local ids = {}
				for name, enabled in value do
					local id = PotionIdByName[name]
					if id and enabled then
						ids[id] = true
					end
				end
				Config.BuyPotionIds = ids
			end,
		})
		local PotionLabel = PotionGroup:AddLabel("-", true)
		uiLoop(function()
			local quests = State.Quests
			for index = 1, 3 do
				local slot = type(quests) == "table" and type(quests.slots) == "table" and quests.slots[tostring(index)] or nil
				local text = "-"
				local color = GREY
				if type(quests) == "table" and quests.locked == true then
					text = "finish the tutorial first"
				elseif type(slot) == "table" then
					local progress, target = tonumber(slot.progress) or 0, tonumber(slot.target) or 0
					text = string.format("%s %s / %s", tostring(slot.kind or "?"), formatNumber(progress), formatNumber(target))
					if target > 0 and progress >= target then
						text ..= "  ready"
						color = GREEN
					end
				end
				setLabel(QuestLabels[index], field(DIFFICULTY_NAMES[index], text, color))
			end
			local upgrades = State.Upgrades
			if type(upgrades) == "table" and upgrades.unlocked == true and UpgradeRules then
				local lines = {}
				local levels = type(upgrades.levels) == "table" and upgrades.levels or {}
				local prices = type(UpgradeRules.Prices) == "table" and UpgradeRules.Prices or {}
				local maxima = type(UpgradeRules.Max) == "table" and UpgradeRules.Max or {}
				for _, key in GEM_KEYS do
					local level = tonumber(levels[key]) or 0
					local cap = tonumber(maxima[key]) or 0
					local list = prices[key]
					local price = type(list) == "table" and list[level + 1] or nil
					local nextText = level >= cap and "max" or (price and (formatNumber(price) .. " gems") or "-")
					table.insert(lines, field(GEM_LABELS[key], string.format("%d/%d, next %s", level, cap, nextText), Config.GemKeys[key] and BLUE or GREY))
				end
				setLabel(GemLabel, table.concat(lines, "\n"))
			else
				setLabel(GemLabel, colored("Unlocks after your first rebirth", GREY))
			end
			local potions = State.Potions
			if type(potions) == "table" then
				local elapsed = os.clock() - State.PotionsAt
				local activeParts, ownedParts = {}, {}
				local inventory = type(potions.inventory) == "table" and potions.inventory or {}
				local active = type(potions.active) == "table" and potions.active or {}
				for _, id in PotionOrder do
					local remaining = (tonumber(active[id]) or 0) - elapsed
					if remaining > 0 then
						table.insert(activeParts, PotionLabels[id] .. " " .. formatDuration(remaining))
					end
					local count = tonumber(inventory[id]) or 0
					if count > 0 then
						table.insert(ownedParts, PotionLabels[id] .. " x" .. count)
					end
				end
				setLabel(PotionLabel, field("Active", #activeParts > 0 and table.concat(activeParts, ", ") or "none", GREEN) .. "\n" .. field("Owned", #ownedParts > 0 and table.concat(ownedParts, ", ") or "none", BLUE))
			end
		end)
	end
	setupExtrasTab()
	local function setupSettings()
		local connections = {}
		local effects = setmetatable({}, { __mode = "k" })
		local scanThread = nil
		local renderDefaults
		local reconnecting = false
		local reconnectToken = 0
		local triggers = 0
		local lastPulse = os.clock()
		local MenuGroup = Tabs.Settings:AddLeftGroupbox("Menu", "logs")
		MenuGroup:AddToggle("AntiAfk", { Text = "Anti-AFK", Default = true })
		local AfkLabel = MenuGroup:AddLabel("AFK triggers: 0")
		local function pulse()
			local camera = Workspace.CurrentCamera
			if not camera or not callable(VirtualUser.CaptureController) or not callable(VirtualUser.ClickButton2) then
				return false
			end
			local ok = pcall(function()
				VirtualUser:CaptureController()
				VirtualUser:ClickButton2(Vector2.new(0, 0), camera.CFrame)
			end)
			if not ok then
				return false
			end
			triggers += 1
			lastPulse = os.clock()
			restoreIdentity()
			pcall(setLabel, AfkLabel, "AFK triggers: " .. triggers)
			return true
		end
		local function applyPause(enabled)
			pcall(function()
				GuiService:SetGameplayPausedNotificationEnabled(not enabled)
			end)
			pcall(function()
				local notification = CoreGui:FindFirstChild("RobloxNetworkPauseNotification")
				if notification then
					notification.Enabled = not enabled
				end
			end)
			if not enabled then
				return
			end
			pcall(function()
				local paused
				if typeof(gethiddenproperty) == "function" then
					paused = gethiddenproperty(LocalPlayer, "GameplayPaused")
				else
					paused = LocalPlayer.GameplayPaused
				end
				if paused ~= true then
					return
				end
				if typeof(sethiddenproperty) == "function" then
					sethiddenproperty(LocalPlayer, "GameplayPaused", false)
				else
					LocalPlayer.GameplayPaused = false
				end
			end)
		end
		local function stripEffect(instance)
			if instance.ClassName == "ParticleEmitter" or instance.ClassName == "Trail" or instance.ClassName == "Smoke" or instance.ClassName == "Fire" or instance.ClassName == "Sparkles" or instance.ClassName == "Explosion" or instance.ClassName == "Beam" then
				if effects[instance] == nil then
					effects[instance] = instance.Enabled
				end
				pcall(function()
					instance.Enabled = false
				end)
			end
		end
		local function restoreEffects()
			if scanThread and coroutine.status(scanThread) ~= "dead" and scanThread ~= coroutine.running() then
				pcall(task.cancel, scanThread)
			end
			scanThread = nil
			for instance, value in effects do
				if instance.Parent then
					pcall(function()
						instance.Enabled = value
					end)
				end
			end
			table.clear(effects)
			if renderDefaults then
				pcall(function()
					settings().Rendering.QualityLevel = renderDefaults.Quality
				end)
				Lighting.GlobalShadows = renderDefaults.Shadows
				Lighting.FogEnd = renderDefaults.Fog
				renderDefaults = nil
			end
		end
		MenuGroup:AddToggle("AntiGameplayPause", { Text = "No Gameplay Paused", Default = true })
		MenuGroup:AddToggle("AutoReconnect", { Text = "Auto Reconnect on Kick", Default = false })
		MenuGroup:AddToggle("Disable3D", {
			Text = "Disable 3D Rendering",
			Default = false,
			Callback = function(value)
				pcall(function()
					RunService:Set3dRenderingEnabled(not value)
				end)
			end,
		})
		MenuGroup:AddToggle("FpsBoost", {
			Text = "FPS Boost",
			Default = false,
			Callback = function(value)
				if value then
					if not renderDefaults then
						renderDefaults = {
							Quality = settings().Rendering.QualityLevel,
							Shadows = Lighting.GlobalShadows,
							Fog = Lighting.FogEnd,
						}
					end
					pcall(function()
						settings().Rendering.QualityLevel = Enum.QualityLevel.Level01
					end)
					Lighting.GlobalShadows = false
					Lighting.FogEnd = 9e9
					if not scanThread or coroutine.status(scanThread) == "dead" then
						scanThread = task.spawn(function()
							for index, instance in Workspace:GetDescendants() do
								if not Toggles.FpsBoost.Value or not Farm.Running then
									break
								end
								pcall(stripEffect, instance)
								if index % 2000 == 0 then
									task.wait()
								end
							end
							scanThread = nil
						end)
					end
				else
					restoreEffects()
				end
			end,
		})
		MenuGroup:AddToggle("HideUiOnStart", { Text = "Hide UI On Start", Default = false })
		MenuGroup:AddLabel("Menu bind"):AddKeyPicker("MenuKeybind", {
			Default = "RightShift",
			NoUI = true,
			Text = "Menu keybind",
		})
		Library.ToggleKeybind = Options.MenuKeybind
		applyPause(true)
		local ScriptGroup = Tabs.Settings:AddLeftGroupbox("Script", "terminal")
		ScriptGroup:AddButton({
			Text = "Unload Script",
			Func = function()
				Library:Unload()
			end,
		})
		Toggles.AntiGameplayPause:OnChanged(function()
			applyPause(Toggles.AntiGameplayPause.Value)
		end)
		table.insert(connections, LocalPlayer.Idled:Connect(function()
			if Toggles.AntiAfk.Value and not Library.Unloaded then
				pulse()
			end
		end))
		table.insert(connections, Workspace.DescendantAdded:Connect(function(instance)
			if Toggles.FpsBoost.Value then
				stripEffect(instance)
			end
		end))
		local function rejoin(fresh)
			if reconnecting or Library.Unloaded or not Toggles.AutoReconnect.Value then
				return
			end
			reconnecting = true
			local token = reconnectToken
			queueSelf()
			local ok = pcall(function()
				if fresh then
					TeleportService:Teleport(game.PlaceId, LocalPlayer)
				else
					TeleportService:TeleportToPlaceInstance(game.PlaceId, game.JobId, LocalPlayer)
				end
			end)
			if not ok then
				reconnecting = false
				if not fresh and token == reconnectToken then
					task.delay(1.5, function()
						if token == reconnectToken then
							rejoin(true)
						end
					end)
				end
			end
		end
		table.insert(connections, TeleportService.TeleportInitFailed:Connect(function(player)
			if player == LocalPlayer and reconnecting then
				reconnecting = false
				local token = reconnectToken
				task.delay(3, function()
					if token == reconnectToken then
						rejoin(true)
					end
				end)
			end
		end))
		spawnLoop(function()
			local promptGui = CoreGui:WaitForChild("RobloxPromptGui", 30)
			local overlay = promptGui and promptGui:WaitForChild("promptOverlay", 30)
			if Library.Unloaded or not overlay then
				return
			end
			table.insert(connections, overlay.ChildAdded:Connect(function(child)
				if child.Name == "ErrorPrompt" then
					rejoin(false)
				end
			end))
		end)
		spawnLoop(function()
			while Farm.Running and not Library.Unloaded do
				restoreIdentity()
				if Toggles.AntiGameplayPause.Value then
					applyPause(true)
				end
				if Toggles.AntiAfk.Value and os.clock() - lastPulse >= 60 then
					pulse()
				end
				task.wait(1)
			end
		end)
		track(function()
			reconnectToken += 1
			for _, connection in connections do
				connection:Disconnect()
			end
			applyPause(false)
			restoreEffects()
			pcall(function()
				RunService:Set3dRenderingEnabled(true)
			end)
		end)
	end
	setupSettings()
	ThemeManager:SetLibrary(Library)
	ThemeManager:SetFolder("OuroborosHub")
	local hasDefault = false
	pcall(function()
		hasDefault = typeof(isfile) == "function" and isfile("OuroborosHub/themes/default.txt")
	end)
	if not hasDefault then
		ThemeManager:SaveDefault("Evil Hello Kitty")
	end
	ThemeManager:ApplyToTab(Tabs.Settings)
	SaveManager:SetLibrary(Library)
	SaveManager:IgnoreThemeSettings()
	SaveManager:SetIgnoreIndexes({ "MenuKeybind" })
	SaveManager:SetFolder("OuroborosHub/MergeHackers")
	SaveManager:BuildConfigSection(Tabs.Settings)
	ThemeManager:LoadDefault()
	SaveManager:LoadAutoloadConfig()
	if Toggles.HideUiOnStart.Value then
		Library:Toggle(false)
	end
	return Window, Tabs
end

local okInterface, interfaceError = pcall(setupInterface)
if not okInterface then
	fail("interface: " .. tostring(interfaceError))
	warn("[Ouroboros] " .. tostring(Farm.LastError))
end

notify("Ouroboros", "Merge Hackers autofarm is running", 5)
