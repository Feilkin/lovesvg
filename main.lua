local lovesvg = require "lovesvg"

local img
local x,y, xx, yy = math.huge,math.huge, -math.huge,-math.huge

function love.load(arg)
	local filename = arg[1] and (arg[1] ~= ".") or arg[2] or "love.svg"
	img = lovesvg.loadSVG(filename, { depth = 4, discard_distance = 0.00001 })

	-- this is just so we can center the image to the screen
	-- this will probably be moved to lovesvg
	local min, max = math.min, math.max
	local function findAABB(obj)
		if obj.renderer then
			if not obj.renderer.AABB then
				print('No AABB (triangulation failed):' .. obj.label .. '#' .. obj.attributes.id)
			else
				x = min(obj.renderer.AABB[1], x)
				y = min(obj.renderer.AABB[2], y)
				xx = max(obj.renderer.AABB[3], xx)
				yy = max(obj.renderer.AABB[4], yy)
			end
		end
		for _, child in obj:ichildren() do
			findAABB(child)
		end
	end
	findAABB(img)

	love.graphics.setBackgroundColor(64, 64, 64, 255)
end

function love.draw()
	local cw, ch = love.graphics.getDimensions()
	-- nice little checkers background
	love.graphics.setColor(96, 96, 96, 255)
	for y = 0, ch, 8 do
		for x = 0, cw, 16 do
			if y % 16 == 0 then
				love.graphics.rectangle("fill", x, y, 8, 8)
			else
				love.graphics.rectangle("fill", x + 8, y, 8, 8)
			end
		end
	end
	love.graphics.setColor(255, 255, 255, 255)

	-- center the image to the screen, zoom to fit
	local iw, ih = xx - x, yy - y

	local s = math.min(cw / iw, ch / ih)

	local ox = (cw - ((iw + x)* s)) / 2
	local oy = (ch - ((ih + y)* s)) / 2

	love.graphics.translate(ox, oy)
	love.graphics.scale(s, s)
	img:draw()
end