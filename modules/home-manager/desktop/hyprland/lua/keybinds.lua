local cmd = {
	drun = "wofi -G --show drun",
	read_cliphist = "cliphist list | wofi --dmenu | cliphist decode | wl-copy",
	sr = "wf-toggle",
	ss = "grimblast copysave area ~/Pictures/Screenshots/s_$(date +%Y%m%d-%H%M%S).png",
}

local K = {
	mod = "SUPER",
	ctrl = "CTRL",
	shift = "SHIFT",
	mod_shift = "SUPER + SHIFT",
	mod_ctrl = "SUPER + CTRL",
}

-- i am NOT interested in typing out `mod .. " + [key]"
-- 30,000 times
local function chord(m, k)
	return m .. " + " .. k
end

local function tags_contain(needle, haystack)
	for _, val in pairs(haystack) do
		if val == needle then
			return true
		end
	end

	return false
end

local function ensure_floating()
	local target = hl.get_active_window()
	if target ~= nil and not target.floating then
		if tags_contain("floatlocked", target.tags) then
			hl.dispatch(hl.dsp.window.tag({ tag = "-floatlocked" }))
		else
			hl.dispatch(hl.dsp.window.float({ action = "on" }))
		end
	end
end

hl.bind(chord(K.mod_shift, "E"), hl.dsp.exit())

for i = 1, 10 do
	local key = tostring(i % 10)
	hl.bind(chord(K.mod, key), hl.dsp.focus({ workspace = i, on_current_monitor = true }))
	hl.bind(chord(K.mod_shift, key), hl.dsp.window.move({ workspace = i, follow = false }))
	hl.bind(chord(K.mod_ctrl, key), hl.dsp.window.move({ workspace = i, follow = true }))
end

hl.bind(chord(K.mod, "Q"), hl.dsp.window.close())
hl.bind(chord(K.mod_shift, "Q"), hl.dsp.window.kill())
hl.bind(chord(K.mod_shift, "P"), hl.dsp.window.pin())

hl.bind(chord(K.mod, "mouse:272"), function()
	hl.dispatch(hl.dsp.window.tag({ tag = "+floatlocked" }))
	hl.dispatch(hl.dsp.window.float({ action = "off" }))
end, { mouse = true, click = true })

hl.bind(chord(K.mod, "mouse:272"), function()
	ensure_floating()
	hl.dispatch(hl.dsp.window.drag())
end, { mouse = true })

hl.bind(chord(K.mod, "mouse:273"), hl.dsp.window.resize())
hl.bind(chord(K.mod, "F"), hl.dsp.window.fullscreen({ mode = "fullscreen", action = "toggle" }))

hl.bind(chord(K.mod, "H"), hl.dsp.focus({ direction = "left" }))
hl.bind(chord(K.mod, "J"), hl.dsp.focus({ direction = "down" }))
hl.bind(chord(K.mod, "K"), hl.dsp.focus({ direction = "up" }))
hl.bind(chord(K.mod, "L"), hl.dsp.focus({ direction = "right" }))
hl.bind(chord(K.mod_shift, "H"), hl.dsp.window.move({ direction = "left" }))
hl.bind(chord(K.mod_shift, "J"), hl.dsp.window.move({ direction = "down" }))
hl.bind(chord(K.mod_shift, "K"), hl.dsp.window.move({ direction = "up" }))
hl.bind(chord(K.mod_shift, "L"), hl.dsp.window.move({ direction = "right" }))

hl.bind(chord(K.mod, "Return"), hl.dsp.exec_cmd(A.term))
hl.bind(chord(K.mod, "W"), hl.dsp.exec_cmd(A.browser))
hl.bind(chord(K.mod, "P"), hl.dsp.exec_cmd(A.sound))

hl.bind(chord(K.mod, "D"), hl.dsp.exec_cmd(cmd.drun))
hl.bind(chord(K.mod, "V"), hl.dsp.exec_cmd(cmd.read_cliphist))
hl.bind(chord(K.mod, "S"), hl.dsp.exec_cmd(cmd.ss))
hl.bind(chord(K.mod_shift, "S"), hl.dsp.exec_cmd(cmd.sr))
