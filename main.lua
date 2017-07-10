local lovesvg = require "lovesvg"

local img
local x,y, xx, yy = math.huge,math.huge, -math.huge,-math.huge

local file_pattern = "^.*%.svg$"
local files = {}

local function findFiles(folder)
	local items = love.filesystem.getDirectoryItems(folder)
	local out = {}

	for _, item in ipairs(items) do
		if item:match(file_pattern) then
			table.insert(out, item)
		end
	end

	return item
end

function love.load(arg)
	local filename = arg[1] and (arg[1] ~= ".") or arg[2] or "love.svg"
	img = lovesvg.loadSVG(filename, { depth = 4, discard_distance = 0.00001 })

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
	local x,y, xx,yy = unpack(img.attributes.viewBox)
	local iw, ih = xx - x, yy - y

	local s = math.min(cw / iw, ch / ih)

	local ox = (cw - ((iw + x)* s)) / 2
	local oy = (ch - ((ih + y)* s)) / 2

	love.graphics.translate(ox, oy)
	love.graphics.scale(s, s)
	img:draw()
end