swayimg.text.visible = false
swayimg.mode = "viewer"

-- swayimg.viewer.default_scale = "optimal"
-- swayimg.viewer.set_fix_scale = "fit"
swayimg.viewer.default_position = "center"

local function fit()
	local img = swayimg.viewer.get_image()
	if img ~= nil then
		local w = img.width
		local h = img.height

		local max_h = math.floor(1440 * 0.8)
		local max_w = math.floor(2560 * 0.8)

		if w > max_w then
			w = max_w
		end
		if h > max_h then
			h = max_h
		end
		return { w = w, h = h }
	end
end

local function callback()
	swayimg.mode = "viewer"

	local clamped = fit()
	swayimg.set_window_size(clamped.w, clamped.h)
  swayimg.viewer.set_window_background("mirror")
end

swayimg.on_initialized(callback)
