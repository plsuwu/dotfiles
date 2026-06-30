hl.monitor({
	output = "desc:Acer Technologies XB323U TKWSA0018523",
	mode = "2560x1440@170.02Hz",
	position = "0x0",
	cm = "auto",
})

hl.monitor({
	output = "desc:Dell Inc. DELL S2721DGF GY2PS83",
	mode = "2560x1440@165.08Hz",
	position = "0x-1440",
	cm = "auto",
})

hl.monitor({
	output = "desc:Samsung Electric Company Odyssey G40B HNMW300577",
	mode = "1920x1080@239.76Hz",
	position = "-1080x-720",
	transform = 3,
	cm = "auto",
})

hl.window_rule({
	name = "fix-xwayland-drags",
	match = {
		class = "^$",
		title = "^$",
		xwayland = true,
		float = true,
		fullscreen = false,
		pin = false,
	},

	no_focus = true,
})
