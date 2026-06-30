for _, path in ipairs(require("nix-plugin")) do
	hl.plugin.load(path)
end

hl.config({
	plugin = {
		hyprbars = {
			-- enabled = true,
			enabled = false,
			bar_height = 28,
			bar_text_size = 14,
			bar_text_font = "Inter",
			bar_buttons_alignment = "left",
			bar_part_of_window = true,
			["col.text"] = "rgb(f5f5f7)",
			on_double_click = "hyprctl dispatch 'hl.dsp.window.fullscreen()'",

			bar_padding = 12,

			bar_color = "rgb(1d1d1f)",
			inactive_button_color = "rgb(666666)",
		},
	},
})

hl.plugin.hyprbars.add_button({
	bg_color = "rgb(ff605c)",
	fg_color = "rgb(ffffff)",
	size = 11,
	icon = "",
	action = "hyprctl dispatch 'hl.dsp.window.close()'",
})
