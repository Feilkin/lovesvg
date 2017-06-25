local lovesvg = require "lovesvg"

local abm
local x,y, xx, yy = math.huge,math.huge, 0,0

function love.load()
	abm = lovesvg.loadSVG("logo_abm.svg", { depth = 2, discard_distance = 0.00001 })

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
	findAABB(abm)
end

function love.draw()
	-- center the image to the screen, zoom to fit
	local cw, ch = love.graphics.getDimensions()
	local iw, ih = xx - x, yy - y

	local s = math.min(cw / iw, ch / ih)

	love.graphics.scale(s, s)
	love.graphics.translate(-x, -y)
	abm:draw()
end