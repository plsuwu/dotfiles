local HOME = os.getenv("HOME")
local PUBLIC = HOME .. "/src/dotfiles/modules/home-manager/desktop/hyprland/lua"

package.path = package.path .. ";" .. PUBLIC .. "/?.lua"

hl.env("XCURSOR_SIZE", "32")
hl.env("HYPRCURSOR_SIZE", "32")
hl.env("ELECTRON_OZONE_PLATFORM_HINT", "wayland")
hl.env("XDG_SESSION_TYPE", "wayland")
hl.env("XDG_CURRENT_DESKTOP", "Hyprland")
hl.env("XDG_SESSION_DESKTOP", "Hyprland")

-- this doesn't work as well as i had hoped but its
-- kind of useful i suppose...
local function get_store_path(pkg)
	local cmd = string.format("realpath $(which %s)", pkg)
	local f = assert(io.popen(cmd, "r"))
	local out = f:read("*a")
	f:close()

	return out
end

A = {
	term = get_store_path("alacritty"),
	browser = get_store_path("brave"),
	files = get_store_path("nemo"),
	wofi = get_store_path("wofi"),
	clip = get_store_path("cliphist"),
	ss = get_store_path("grimblast"),
	sr = get_store_path("wf-recorder"),
	sound = get_store_path("pwvucontrol"),
	vesktop = get_store_path("vesktop"),
}

hl.on("hyprland.start", function()
	hl.exec_cmd("systemctl --user start hyprland-session.target")
  hl.exec_cmd("steam -silent")

	-- some of this block could be implemented as systemd units instead
	-- not sure that is more convenient though...
	hl.exec_cmd("awww-daemon --no-cache")
	hl.exec_cmd("awww img /home/please/.config/wallpaper.jpg")
	hl.exec_cmd("wl-paste --type text --watch cliphist store")
	hl.exec_cmd("wl-paste --type image --watch cliphist store")
	-- --

	hl.exec_cmd(A.vesktop)
end)

hl.config({
	misc = {
		disable_hyprland_logo = true,
		disable_splash_rendering = true,
	},
	binds = {
		drag_threshold = 5,
	},

	general = {
		border_size = 1,
		-- border_color = "E1DFE1",
		gaps_in = 1,
		gaps_out = 0,
		layout = "dwindle",
		col = {
			active_border = "rgba(E1DFE14f)",
		},
    modal_parent_blocking = false,
	},

	decoration = {
		rounding = 8,
	},

	input = {
		numlock_by_default = true,
		repeat_rate = 50,
		repeat_delay = 200,
		sensitivity = 0.5,
		accel_profile = "flat",
	},

	animations = {
		enabled = false,
	},
})

require("layout")
require("rules")
require("keybinds")
require("plugins")
