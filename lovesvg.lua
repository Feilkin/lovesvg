local lovesvg = {
	_VERSION     = 'lovesvg v0.0.0',
	_DESCRIPTION = 'SVG images as löve meshes',
	_URL         = 'https://github.com/Feilkin/lovesvg',
	_LICENCE     = [[
		MIT LICENSE


	    Copyright (c) 2017 Aatu Hieta

	    Permission is hereby granted, free of charge, to any person obtaining a
	    copy of this software and associated documentation files (the
	    "Software"), to deal in the Software without restriction, including
	    without limitation the rights to use, copy, modify, merge, publish,
	    distribute, sublicense, and/or sell copies of the Software, and to
	    permit persons to whom the Software is furnished to do so, subject to
	    the following conditions:
	    The above copyright notice and this permission notice shall be included
	    in all copies or substantial portions of the Software.
	    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
	    OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
	    MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
	    IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
	    CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
	    TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
	    SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
	]]
}

-- TODO: coroutine loader

-- This module requires Löve (https://love2d.org)
if not love then
	error("love not defined")
end


--- Parses XML file.
-- http://lua-users.org/wiki/LuaXml (20.6.2017)
-- @param s string contents of XML file
-- @return a lua table
local function _parseXML(s)
	local insert = table.insert
	local remove = table.remove
	local find = string.find
	local gsub = string.gsub
	local sub = string.sub

	local parseargs = function (s)
	  local arg = {}
	  gsub(s, "([%-%w]+)=([\"'])(.-)%2", function (w, _, a)
	    arg[w] = a
	  end)
	  return arg
	end
	local stack = {}
	local top = {}
	insert(stack, top)
	local ni,c,label,xarg, empty
	local i, j = 1, 1
	while true do
		ni,j,c,label,xarg, empty = find(s, "<(%/?)([%w:_-]+)(.-)(%/?)>", i)
		if not ni then break end
		local text = sub(s, i, ni-1)
		if not find(text, "^%s*$") then
			insert(top, text)
		end
		if empty == "/" then  -- empty element tag
			insert(top, {label=label, xarg=parseargs(xarg), empty=1})
		elseif c == "" then   -- start tag
			top = {label=label, xarg=parseargs(xarg)}
			insert(stack, top)   -- new level
		else  -- end tag
			local toclose = remove(stack)  -- remove top
			top = stack[#stack]
			if #stack < 1 then
				error("nothing to close with "..label)
			end
			if toclose.label ~= label then
				error("trying to close "..toclose.label.." with "..label)
			end
			insert(top, toclose)
		end
		i = j+1
	end
	local text = sub(s, i)
	if not find(text, "^%s*$") then
		insert(stack[#stack], text)
	end
	if #stack > 1 then
		error("unclosed "..stack[#stack].label)
	end
	return stack[1]
end

--- Parses element style CSS
-- TODO: make a proper CSS parses
local _parseCSSValue, _parseCSS
do
	local pattern = "%s*([^:%s]+)%s*:%s*([^;]+)%s*;"
	local end_pattern = pattern:sub(1, pattern:len() - 1) .. "$"

	local parsers = {
		["#(%x%x%x%x%x%x)"] = function (v)
			local r = tonumber(v:sub(1, 2), 16)
			local g = tonumber(v:sub(3, 4), 16)
			local b = tonumber(v:sub(5, 6), 16)

			return { r, g, b,  255 }
		end,
		["%-?%d+%.?%d*"] = function (v)
			return tonumber(v)
		end,
		["none"] = function (v) return nil end,
	}

	_parseCSSValue = function (v)
		for pattern, f in pairs(parsers) do
			local c = v:match(pattern)
			if c then return f(c) end
		end

		print("I don't know how to parse " .. v)
		return nil
	end

	_parseCSS = function (css)
		local style = {}

		-- load key: value; pairs into the style table
		local raw = {}
		for k, v in css:gmatch(pattern) do
			table.insert(raw, {k, v})
		end

		do
			local k, v = css:match(end_pattern)
			table.insert(raw, {k, v})
		end

		-- parse the values into something usefull
		for i, r in ipairs(raw) do
			local k, v = r[1], r[2]
			v = _parseValue(v)

			style[k] = v
		end

		return style
	end
end

--- Transformations and stuff?
local function _popStyle()
end

local function _pushStyle(s)
end

local function _setColor(s, colorKey)
	if not s then return end

	if s[colorKey] then
		love.graphics.setColor(s[colorKey])
	else
	    love.graphics.setColor(0, 0, 0, 255)
	end
	if s["opacity"] then
		local r, g, b, a = love.graphics.getColor()
		love.graphics.setColor(r, g, b, s["opacity"] * 255)
	end
end

-- Base for Object
local _Object = {
	ichildren = function(self)
		local i = 0
		local t = self.children
		local n = table.getn(t)
		return function ()
			i = i + 1
			if i <= n then return i, t[i] end
		end
	end,


	render = function (self, opts)
		if self.renderer then
			self.renderer:render(opts)
		end

		for i, child in self:ichildren() do
			child:render(opts)
		end
	end,

	draw = function(self)
		-- recursive draw function

		--[[
		local oldStyle = _popStyle()
		_pushStyle(self.attributes.style)

		if self.attributes.style then
			_pushStyle(self.attributes.style)
		end
		--]]

		if self.renderer then
			self.renderer:draw(self.attributes.style)
		end

		for i, child in self:ichildren() do
			child:draw()
		end

		--_pushStyle(oldStyle)
	end
}

--- Creates new, empty Object.
-- Objects can contain polygons, child objects, and attributes
-- @return Object
local function _newObject()
	local obj = {
		attributes = {},
		children = {},
		renderer = nil
	}

	setmetatable(obj, { __index = _Object })

	return obj
end

--- Sets attributes from stack to Object.
-- TODO: is this the best way to do this??
-- @param obj Object the attributes should be assigned to
-- @param stack current element of the stack, as returned by _parseXML
-- @see _newObject _parseXML
local _setAttributes
do
	local _count = 0
	local function _idCounter()
		_count = _count + 1
		return _count
	end

	local override_style_args = {
		"fill",
		"opacity",
		"stroke",
		"stroke-width",
		"width",
		"height",
	}

	_setAttributes = function (obj, stack)
		for k, v in pairs(stack.xarg) do
			obj.attributes[k] = v
		end

		if not obj.attributes.id then
			-- make up a ID
			obj.attributes.id = string.format("%s%d", obj.label, _idCounter())
		end

		if obj.attributes.style then
			obj.attributes.style = _parseCSS(obj.attributes.style)
		else
			obj.attributes.style = {}
		end

		for _, k in ipairs(override_style_args) do
			if obj.attributes[k] then
				print("overrode " .. k .. " on " .. obj.attributes.id)
				obj.attributes.style[k] = _parseCSSValue(obj.attributes[k])
			end
		end

		return obj
	end
end

-- base for Renderer
local _PathRenderer = {
	options= { depth = 2 },
	render = function (self, options)
		options = options or self.options
		local insert = table.insert

		local vertices = {}
		local cursor   = { x = 0, y = 0, last_command_was_move = false }

		for i, token in ipairs(self.tokenstream) do
			local points, was_move = self.render_functions[token[1]](token[2], cursor,
				options)
			cursor.last_command_was_move = was_move

			if points and (not (cursor.start_x and cursor.start_y)) then -- or cursor.last_command_was_move or true then
				cursor.start_x = points[1]
				cursor.start_y = points[2]
			end

			if points then
				for i = 1, #points, 2 do
					insert(vertices, points[i])
					insert(vertices, points[i + 1])
				end
			end
		end

		-- discard points too close to each other
		local t = {}
		local discard_distance = options.discard_distance or 0.000001

		for i = 1, #vertices, 2 do
			local discard = false
			local ax, ay = vertices[i], vertices[i + 1]

			if #t > 0 then
				local bx, by = t[#t-1], t[#t]
				if math.sqrt((bx - ax)^2 + (by - ay)^2) < discard_distance then
					discard = true
				end
			end

			if not discard then
				table.insert(t, ax)
				table.insert(t, ay)
			end
		end

		-- check the last one is not too close to the first one
		do
			if #t < 6 then
				error("not enough points: " .. #t)
			end
			local discard = false
			local ax, ay = t[1], t[2]

			local bx, by = t[#t-1], t[#t]
			if math.sqrt((bx - ax)^2 + (by - ay)^2) < discard_distance then
				discard = true
			end

			if discard then
				table.remove(t, #t)
				table.remove(t, #t)
			end
		end

		vertices = t

		self.polygon = vertices

		-- triangulate
		local success, triangles = pcall(love.math.triangulate, vertices)

		if success then
			vertices = {}
			-- calculate the AABB for this mesh
			local minx, miny, maxx, maxy = math.huge,math.huge, 0,0

			for _, triangle in ipairs(triangles) do
				if triangle[1] < minx then minx = triangle[1] end
				if triangle[2] < miny then miny = triangle[2] end
				if triangle[3] > maxx then maxx = triangle[3] end
				if triangle[4] > maxy then maxy = triangle[4] end

				insert(vertices, {
					triangle[1], triangle[2],
					0,0,
					255,255,255,255
					})
				insert(vertices, {
					triangle[3], triangle[4],
					0,0,
					255,255,255,255
					})
				insert(vertices, {
					triangle[5], triangle[6],
					0,0,
					255,255,255,255
					})
			end


			local mesh = love.graphics.newMesh(vertices)
			mesh:setDrawMode("triangles")
			self.triangles = triangles
			self.mesh = mesh
			self.AABB = { minx, miny, maxx, maxy }
			return mesh
		end
	end,

	draw = function (self, style)
		local old_color = { love.graphics.getColor() }
		if self.mesh then
			_setColor(style, "fill")
			love.graphics.draw(self.mesh, 0,0)
		end
		if style.stroke and self.polygon then
			_setColor(style, "stroke")

			local old_width = love.graphics.getLineWidth()
			love.graphics.setLineWidth(style['stroke-width'] or old_width)

			love.graphics.polygon('line', self.polygon)
			love.graphics.setLineWidth(old_width)
		end
		if _DEBUG and self.polygon then
			love.graphics.setColor(0, 0, 0, 128)
			love.graphics.line(self.polygon)

			love.graphics.setColor(255, 0, 0, 128)
			love.graphics.line(self.polygon[1],self.polygon[2],
			                   self.polygon[#self.polygon - 1],
			                   self.polygon[#self.polygon])
			love.graphics.setPointSize(6)
			love.graphics.setColor(255, 0, 255, 128)
			love.graphics.points(self.polygon)
			love.graphics.setColor(0, 255, 0, 128)
			love.graphics.points(self.polygon[1], self.polygon[2])
		end
		love.graphics.setColor(old_color)
	end,

	-- The actual render functions
	render_functions = {
		M = function (args, cursor, options)
			cursor.x, cursor.y = args[1], args[2]
			cursor.start_x, cursor.start_y = nil, nil

			return nil, true
		end,
		m = function (args, cursor, options)
			cursor.x = cursor.x + args[1]
			cursor.y = cursor.y + args[2]
			cursor.start_x, cursor.start_y = nil, nil

			return nil, true
		end,
		L = function (args, cursor, options)
			if cursor.last_command_was_move then
				local t = { cursor.x, cursor.y, args[1], args[2]}
				cursor.x, cursor.y = args[1], args[2]
				return t
			end

			cursor.x, cursor.y = args[1], args[2]
			return { cursor.x, cursor.y }
		end,
		l = function (args, cursor, options)
			if cursor.last_command_was_move then
				local t = { cursor.x, cursor.y,
					cursor.x + args[1], cursor.y + args[2]}
				cursor.x, cursor.y = t[3], t[4]
				return t
			end

			cursor.x, cursor.y = cursor.x + args[1], cursor.y + args[2]
			return { cursor.x, cursor.y }
		end,
		H = function (args, cursor, options)
			if cursor.last_command_was_move then
				local t = { cursor.x, cursor.y, args[1], cursor.y }
				cursor.x, cursor.y = args[1], cursor.y
				return t
			end

			cursor.x, cursor.y = args[1], cursor.y
			return { cursor.x, cursor.y }
		end,
		h = function (args, cursor, options)
			if cursor.last_command_was_move then
				local t = { cursor.x, cursor.y, cursor.x + args[1], cursor.y }
				cursor.x, cursor.y = t[3], cursor.y
				return t
			end

			cursor.x, cursor.y = cursor.x + args[1], cursor.y
			return { cursor.x, cursor.y }

		end,
		V = function (args, cursor, options)
			if cursor.last_command_was_move then
				local t = { cursor.x, cursor.y, cursor.x, args[1] }
				cursor.x, cursor.y = cursor.x, args[1]
				return t
			end

			cursor.x, cursor.y = cursor.x, args[1]
			return { cursor.x, cursor.y }
		end,
		v = function (args, cursor, options)
			if cursor.last_command_was_move then
				local t = { cursor.x, cursor.y, cursor.x, cursor.y + args[1] }
				cursor.x, cursor.y = cursor.x, t[4]
				return t
			end

			cursor.x, cursor.y = cursor.x, cursor.y + args[1]
			return { cursor.x, cursor.y }
		end,
		Z = function (args, cursor, options)
			cursor.x, cursor.y = cursor.start_x, cursor.start_y

			return { cursor.x, cursor.y }
		end,
		z = function (args, cursor, options)
			cursor.x, cursor.y = cursor.start_x, cursor.start_y

			return { cursor.x, cursor.y}
		end,
		C = function (args, cursor, options)
			local curve = love.math.newBezierCurve(
				cursor.x, cursor.y,
				args[1], args[2],
				args[3], args[4],
				args[5], args[6])

			local v = curve:render(options.depth)
			cursor.x, cursor.y = v[#v - 1], v[#v]

			return v
		end,
		c = function (args, cursor, options)
			local curve = love.math.newBezierCurve(
				cursor.x, cursor.y,
				cursor.x + args[1], cursor.y + args[2],
				cursor.x + args[3], cursor.y + args[4],
				cursor.x + args[5], cursor.y + args[6])

			local v = curve:render(options.depth)
			cursor.x, cursor.y = v[#v - 1], v[#v]
			
			return v
		end,
		Q = function (args, cursor, options)
			local curve = love.math.newBezierCurve(
				cursor.x, cursor.y,
				args[1], args[2],
				args[3], args[4])

			local v = curve:render(options.depth)
			cursor.x, cursor.y = v[#v - 1], v[#v]

			return v
		end,
		q = function (args, cursor, options)
			local curve = love.math.newBezierCurve(
				cursor.x, cursor.y,
				cursor.x + args[1], cursor.y + args[2],
				cursor.x + args[3], cursor.y + args[4])

			local v = curve:render(options.depth)
			cursor.x, cursor.y = v[#v - 1], v[#v]
			
			return v
		end,
		T = function (args, cursor, options)
			error("T is not implemented yet!")
		end,
		t = function (args, cursor, options)
			error("t is not implemented yet!")
		end,
	}
}

--- Creates a new PathRenderer
-- @param tokenstream TokenStream this Renderer should use
-- @return a empty Renderer
local function _pathRenderer(tokenstream)
	tokenstream = tokenstream or {}
	local r = {
		tokenstream = tokenstream
	}

	setmetatable(r, { __index = _PathRenderer, __call = r.render })
	return r
end

--- Tokenizes given draw command
-- @param cmdstr Command string to tokenize
-- @return a Token stream
local _tokenize
do
	local gsub = string.gsub
	local len = string.len
	local match = string.match
	local sub = string.sub
	local insert = table.insert

	-- Patterns for tokenizing the draw commands
	local OP_CLASS    = "MmLlHhVvZzCcQqTt"
	local DELIM_CLASS = "%s,"
	local TRASH_CAPTURE = string.format("[%s]*",
		DELIM_CLASS)

	-- TODO: figure out a better way to do numbers
	local ARG_CAPTURE = string.format("^[%s]*%%-?%%d+%%.?%%d*",
		DELIM_CLASS)
	local ARG_CAPTURE_SCIENTIFIC = string.format(
		"^[%s]*%%-?%%d+%%.?%%d*e%%-?%%d+",
		DELIM_CLASS)
	local OP_CAPTURE  = string.format("^[%s]*[%s]",
		DELIM_CLASS, OP_CLASS)

	-- argument counts for each op
	local _arg_count = {
		M = 2,
		m = 2,
		L = 2,
		l = 2,
		H = 1,
		h = 1,
		V = 1,
		v = 1,
		Z = 0,
		z = 0,
		C = 6,
		c = 6,
		Q = 4,
		q = 4,
		T = 2,
		t = 2,
	}

	--- Checks that there are right amount of arguments ahead.
	-- @return true if there are, false if not
	local function peekargs(buffer, count)
		if count == 0 then return true end

		local offset = 0
		for i = 1, count do
			local s, e = buffer:find(ARG_CAPTURE_SCIENTIFIC, offset)

			if not s then
				s, e = buffer:find(ARG_CAPTURE, offset)

				if not s then
					print(("!"):rep(64))
					print(buffer:sub(offset))
					print(("!"):rep(64))
					return false
				end
			end

			offset = e + 1
		end

		return true
	end

	--- Pops the next op from the command string.
	-- @return op, buffer
	local function popop(buffer, lastop)
		if buffer == "" then return false, buffer end

		local op = match(buffer, OP_CAPTURE)
		if not op then
			-- some editor chain commands without repeating the op
			local arg_count = _arg_count[lastop]

			if arg_count == 0 then
				return false, buffer
			end

			if not peekargs(buffer, arg_count) then
				print(buffer)
				error(string.format("Expected command, found '%s' instead",
					match(buffer,
						"^(.*)[" .. DELIM_CLASS .. OP_CLASS .. "]")))
			end
			return lastop, buffer
		end

		-- remove the op from the buffer, and trim trash from it
		buffer = sub(buffer, len(op) + 1)

		if len(op) > 1 then
			op = sub(op, len(op))
		end

		return op, buffer
	end

	--- Pops count number of args from command string.
	-- @return args, buffer
	local function popargs(buffer, count)
		if count == 0 then return nil, buffer end
		local args = {}

		for i = 1, count do
			local arg = match(buffer, ARG_CAPTURE_SCIENTIFIC)

			if not arg then
				arg = match(buffer, ARG_CAPTURE)

				if not arg then
					error(string.format("Expected argument, found '%s' instead",
						match(buffer, "^(.-)[" ..DELIM_CLASS .. OP_CLASS .. "]")))
				end
			end

			buffer = sub(buffer, len(arg) + 1)
			arg = gsub(arg, TRASH_CAPTURE, "")

			if not tonumber(arg) then
				error(string.format("Invalid argument: '%s', expected number",
					tostring(arg)))
			end
			insert(args, tonumber(arg))
		end

		return args, buffer
	end

	_tokenize = function (cmdstr)
		local buffer = cmdstr
		local op
		local tokenstream = {}

		op, buffer = popop(buffer, op)
		while op do
			local arg_count = _arg_count[op]

			if not arg_count then
				error(string.format("Unknown (or unimplemented) command '%s'",
					op))
			end

			local args
			args, buffer = popargs(buffer, arg_count)

			insert(tokenstream, { op, args })
			op, buffer = popop(buffer, op)
		end

		return tokenstream
	end
end

--- Tokenizes given draw command, and constructs a PathRenderer for it.
-- Renderer is a function that returns löve meshes
-- @param cmdstr SVG draw command string
-- @return a Renderer
local function _parseDrawCommand(cmdstr)
	-- Tokenize the command string
	local tokenstream = _tokenize(cmdstr)

	-- Construct the Renderer
	local renderer = _pathRenderer(tokenstream)
	return renderer
end

--- metatable for RectRenderer
local _RectRenderer = {
	render = function (self, opts)
		local x, y = self.x, self.y
		local w, h = self.width, self.height

		local vertices = {
			{x, y, 0,0, 255, 255, 255, 255},
			{x + w, y, 0,0, 255, 255, 255, 255},
			{x + w, y + h, 0,0, 255, 255, 255, 255},
			{x, y + h, 0,0, 255, 255, 255, 255}
		}

		self.polygon = { x,y, x+w,y, x+w,y+h, x,y+h }
		self.AABB = {
			x, y,
			x + w, y + h
		}
	end,

	draw = function (self, style)
		local old_color = { love.graphics.getColor() }
		_setColor(style, "fill")
		love.graphics.rectangle('fill', self.x, self.y, self.width, self.height)


		if style.stroke then
			_setColor(style, "stroke")
			love.graphics.rectangle('line', self.x, self.y, self.width, self.height)
		end

		love.graphics.setColor(old_color)
	end
}

--- constructs a new RectRenderer
-- @param obj a SVG[rect] obj
local function _rectRenderer(obj)
	local r = {
		x = tonumber(obj.attributes.x),
		y = tonumber(obj.attributes.y),
		width = tonumber(obj.attributes.width),
		height = tonumber(obj.attributes.height)
	}
	setmetatable(r, { __index = _RectRenderer, __call = r.render })
	return r
end


--- Metatable for ImageRenderer
local _ImageRenderer = {
	prepareImage = function (self)
		local scheme, rest = self.href:match("^([^:]+)%:(.+)$")

		local img = self.schemeHandlers[scheme](rest)
		self.image = img
	end,

	schemeHandlers = {
		data = function (rest)
			local mime, encoding, data = rest:match("^([^;]+);([^,]+),(.+)$")

			if encoding == "base64" then
				local filedata = love.filesystem.newFileData(data, "name", encoding)
				local imagedata = love.image.newImageData(filedata)
				local image = love.graphics.newImage(imagedata)

				return image
			else
				error(string.format("%s is not supported encoding, make a Issue"))
			end
		end
	},

	render = function (self, options)
		local width = self.obj.attributes.style.width or self.image:getWidth()
		local height = self.obj.attributes.style.height or self.image:getHeight()
		self.AABB = { 0,0, width,height }
	end,

	draw = function (self, style)
		local old_color = { love.graphics.getColor() }
		_setColor(style, "fill")
		love.graphics.draw(self.image, 0, 0)

		love.graphics.setColor(old_color)
	end,
}

--- Constructs a new ImageRenderer
-- @param obj a SVG[image] obj
local function _imageRenderer(obj)
	local href = obj.attributes.href or obj.attributes["xlink:href"]
	local r = {
		href = href,
		obj = obj
	}

	setmetatable(r, { __index = _ImageRenderer })
	r:prepareImage()
	return r
end

--- Walks the Stack, creating a tree of Objects.
-- I have no idea what I am doing
-- @param root root of this branch
-- @param stack stack to walk
-- @return the object passed as root
local function _walkStack(root, stack)
	root.label = stack.label
	if stack.xarg then
		_setAttributes(root, stack)
	end

	if root.label == "path" then
		if root.attributes.d and (root.attributes.d ~= "") then
			local renderer = _parseDrawCommand(root.attributes.d)
			root.renderer = renderer
		end
	elseif root.label == "rect" then
		root.renderer = _rectRenderer(root)
	elseif root.label == "image" then
		root.renderer = _imageRenderer(root)
	end

	if #stack and (#stack > 0) then
		for i = 1, #stack do
			if type(stack[i]) == "table" then
				local child = _newObject()
				_walkStack(child, stack[i])
				table.insert(root.children, child)
			end
		end
	end
end

--- Parses SVG string, returns Object.
-- TODO: description
-- @param contents string contents of SVG
-- @return SVG Object
-- @see _newObject
function lovesvg.parseSVG(contents)
	if (not contents) or (type(contents) ~= "string") then
		error("lovesvg.parseSVG requires string contents as first argument", 2)
	end

	local stack = _parseXML(contents)
	local root = _newObject()

	_walkStack(root, stack)

	return root
end

--- Loads SVG image from file, returns Object that can be drawn
function lovesvg.loadSVG(filename, opts)
	opts = opts or { depth = 2 }
	local contents = love.filesystem.read(filename)
	local root = lovesvg.parseSVG(contents)

	root:render(opts)

	return root
end

return lovesvg