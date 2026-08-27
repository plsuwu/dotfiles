hl.window_rule({
	match = {
		class = "^(swayimg)$",
	},

	float = true,
})

hl.window_rule({
	match = {
		class = "^(vesktop)$",
	},
	workspace = "10",
})

hl.window_rule({
	match = {
		class = "*polkit*",
	},
	float = true,
	pin = true,
})

hl.window_rule({
	match = {
		class = "SwayNotificationCenterControlCenter",
	},
	float = true,
	pin = true,
})

hl.window_rule({
	match = {
		class = "^(xdg-desktop-portal-gtk)$",
	},
	float = true,
	size = { 1200, 700 },
})

hl.window_rule({
	match = {
		class = "com.saivert.pwvucontrol",
	},
	float = true,
	pin = true,
	monitor = 2,
	size = { 1000, 400 },
	move = { 35, 260 },
})

-- idk if this words teehee :3
hl.window_rule({
	match = {
		class = "^(.*):dialog$",
	},
	float = true,
})

-- zoom window rules (possibly life-changing)
-- float all zoom windows so popups don't tile behind the meeting window
hl.window_rule({ match = { class = "^zoom$" }, float = true })

-- avoid stealing focus on open
hl.window_rule({
	match = {
		class = "^zoom$",
		title = "^(Meeting|Zoom Workplace)$",
	},
	no_initial_focus = true,
})

-- keep popups focused so they don't disappear on mouse movement
hl.window_rule({
	match = {
		class = "^zoom$",
		title = "^(menu window|confirm window)$",
	},
	stay_focused = true,
	border_size = 0,
})

hl.window_rule({
	match = {
		class = "^zoom$",
		title = "annotate_toolbar",
	},

	float = true,
	no_initial_focus = true,
	size = { 100, 100 },
	border_size = 0,
})

-- small popups & notifications: force float, center around cursor
hl.window_rule({
	match = {
		class = "^zoom$",
		title = "^(zoom)$",
	},
	float = true,
	no_initial_focus = true,
	move = { "cursor_x-(window_w*0.5)", "cursor_y-(window_h*0.5)" },
	border_size = 0,
})

-- remidner & toast notifications: float pinned bottom-center, dont steal focus
hl.window_rule({
	match = {
		title = "zoom_linux_float_message_reminder",
	},
	no_initial_focus = true,
	float = true,
	move = { "monitor_w*0.5-180", "monitor_h-200" },
	border_size = 0,
})
