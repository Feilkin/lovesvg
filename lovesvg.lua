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

-- Base for Object
local _Object = {
	ichildren = function(self)
		local i = 0
		local n = table.getn(self)
		return function ()
			i = i + 1
			if i <= n then return i, self[i] end
		end
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
local function _setAttributes(obj, stack)
	for k, v in pairs(stack.xarg) do
		obj.attributes[k] = v
	end

	return obj
end

-- base for Renderer
local _Renderer = {
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

			if points then
				for i = 1, #points, 2 do
					insert(vertices, points[i])
					insert(vertices, points[i + 1])
				end
			end
		end

		-- discard points too close to each other
		local t = {}

		for i = 1, #vertices, 2 do
			local discard = false
			local ax, ay = vertices[i], vertices[i + 1]

			if #t > 0 then
				local bx, by = t[#t-1], t[#t]
				if math.sqrt((bx - ax)^2 + (by - ay)^2) < 0.0000001 then
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
			if math.sqrt((bx - ax)^2 + (by - ay)^2) < 0.0000001 then
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

			for _, triangle in ipairs(triangles) do
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
			self.mesh = mesh
			return mesh
		end
	end,

	-- The actual render functions
	render_functions = {
		M = function (args, cursor, options)
			cursor.x, cursor.y = args[1], args[2]

			if not (cursor.start_x and cursor.start_y) then
				cursor.start_x = cursor.x
				cursor.start_y = cursor.y
			end

			return nil, true
		end,
		m = function (args, cursor, options)
			cursor.x = cursor.x + args[1]
			cursor.y = cursor.y + args[2]

			if not (cursor.start_x and cursor.start_y) then
				cursor.start_x = cursor.x
				cursor.start_y = cursor.y
			end

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
			cursor.start_x, cursor.start_y = nil, nil

			return nil, true
		end,
		z = function (args, cursor, options)
			cursor.x, cursor.y = cursor.start_x, cursor.start_y
			cursor.start_x, cursor.start_y = nil, nil

			return nil, true
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
	}
}

--- Creates a new Renderer
-- @param tokenstream TokenStream this Renderer should use
-- @return a empty Renderer
local function _newRenderer(tokenstream)
	tokenstream = tokenstream or {}
	local r = {
		tokenstream = tokenstream
	}

	setmetatable(r, { __index = _Renderer, __call = r.render })
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
	local OP_CLASS    = "MmLlHhVvZzCc"
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
		c = 6
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

--- Tokenizes given draw command, and constructs Renderer for it.
-- Renderer is a function that returns löve meshes
-- @param cmdstr SVG draw command string
-- @return a Renderer
local function _parseDrawCommand(cmdstr)
	-- Tokenize the command string
	local tokenstream = _tokenize(cmdstr)

	-- Construct the Renderer
	local renderer = _newRenderer(tokenstream)
	return renderer
end

--- Walks the Stack, creating a tree of Objects.
-- I have no idea what I am doing
-- @param root root of this branch
-- @param stack stack to walk
-- @return the object passed as root
local function _walkStack(root, stack)
	if stack.xarg then
		_setAttributes(root, stack)
	end

	if root.attributes.d and (root.attributes.d ~= "") then
		local renderer = _parseDrawCommand(root.attributes.d)
		root.renderer = renderer
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
	local stack = _parseXML(contents)
	local root = _newObject()

	_walkStack(root, stack)

	return root
end

return lovesvg