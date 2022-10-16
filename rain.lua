-- title:  rain
-- author: pke1029
-- desc:   A 3D viewer
-- version: 0.1 
-- script: lua


WIDTH = 240
HEIGHT = 136
W2 = 120
H2 = 68
PI = 3.1415
sin = math.sin
cos = math.cos
max = math.max
min = math.min
abs = math.abs
sqrt = math.sqrt
sqrt2 = sqrt(2)
ceil = math.ceil
floor = math.floor
random = math.random
sysmouse = mouse 
systime = time


Umbrella = {
	verts = {{0.11,1.11,-0.76},{-0.25,0.92,-0.87},{-0.64,0.67,-0.9},{0.31,0.94,-0.8},{0.16,0.58,-0.95},{-0.04,0.16,-1.01},{0.54,0.9,-0.68},{0.63,0.49,-0.7},{0.66,0.02,-0.65},{0.67,1,-0.47},{0.89,0.7,-0.28},{1.05,0.34,-0.02},{0.62,1.2,-0.3},{0.79,1.1,0.07},{0.89,0.93,0.5},{0.4,1.21,-0.58},{0.42,1.37,-0.26},{0.38,1.45,0.15},{0.29,1.44,0.61},{0.19,1.41,-0.38},{-0.1,1.54,-0.1},{-0.41,1.58,0.25},{0.06,1.31,-0.59},{-0.36,1.32,-0.52},{-0.8,1.26,-0.38},{-0.27,0.22,0.36},{0.38,1.18,-0.53},{-0.29,0.21,0.35},{0.36,1.18,-0.55},{-0.26,0.2,0.35},{0.39,1.16,-0.54},{-0.27,0.2,0.34},{0.37,1.16,-0.56},{-0.27,0.23,0.38},{-0.3,0.22,0.35},{-0.24,0.19,0.36},{-0.28,0.19,0.33},{-0.3,0.18,0.42},{-0.33,0.18,0.39},{-0.27,0.15,0.4},{-0.31,0.14,0.37},{-0.33,0.13,0.48},{-0.37,0.1,0.48},{-0.3,0.09,0.46},{-0.34,0.07,0.46},{-0.3,0.1,0.56},{-0.33,0.07,0.58},{-0.28,0.06,0.54},{-0.3,0.03,0.56},{-0.24,0.11,0.62},{-0.23,0.09,0.66},{-0.21,0.07,0.6},{-0.21,0.05,0.64},{-0.16,0.16,0.62},{-0.13,0.16,0.66},{-0.14,0.12,0.6},{-0.11,0.12,0.64},{-0.12,0.22,0.58},{-0.08,0.23,0.59},{-0.09,0.18,0.56},{-0.05,0.2,0.57},{0.11,1.11,-0.76},{-0.25,0.92,-0.87},{-0.64,0.67,-0.9},{0.31,0.94,-0.8},{0.16,0.58,-0.95},{-0.04,0.16,-1.01},{0.54,0.9,-0.68},{0.63,0.49,-0.7},{0.66,0.02,-0.65},{0.67,1,-0.47},{0.89,0.7,-0.28},{1.05,0.34,-0.02},{0.62,1.2,-0.3},{0.79,1.1,0.07},{0.89,0.93,0.5},{0.4,1.21,-0.58},{0.42,1.37,-0.26},{0.38,1.45,0.15},{0.29,1.44,0.61},{0.19,1.41,-0.38},{-0.1,1.54,-0.1},{-0.41,1.58,0.25},{0.06,1.31,-0.59},{-0.36,1.32,-0.52},{-0.8,1.26,-0.38},{-1.06,0,-1.36},{-1.5,-0,-0.85},{-1.71,-0,-0.21},{-1.66,-0,0.46},{-1.36,0,1.06},{-0.85,0,1.5},{-0.21,0,1.71},{0.46,0,1.66},{1.06,-0,1.36},{1.5,0,0.85},{1.71,0,0.21},{1.66,0,-0.46},{1.36,-0,-1.06},{0.85,-0,-1.5},{0.21,0,-1.71},{-0.46,0,-1.66},},
	faces = {{3,2,5,6},{1,16,4},{2,1,4,5},{5,4,7,8},{6,5,8,9},{4,16,7},{8,7,10,11},{9,8,11,12},{7,16,10},{12,11,14,15},{10,16,13},{11,10,13,14},{15,14,18,19},{13,16,17},{14,13,17,18},{18,17,20,21},{19,18,21,22},{17,16,20},{21,20,23,24},{22,21,24,25},{20,16,23},{25,24,2,3},{23,16,1},{24,23,1,2},{26,27,29,28},{28,29,33,32},{32,33,31,30},{30,31,27,26},{30,26,34,36},{33,29,27,31},{37,36,40,41},{28,32,37,35},{26,28,35,34},{32,30,36,37},{40,38,42,44},{34,35,39,38},{36,34,38,40},{35,37,41,39},{45,44,48,49},{39,41,45,43},{41,40,44,45},{38,39,43,42},{48,46,50,52},{42,43,47,46},{44,42,46,48},{43,45,49,47},{51,53,57,55},{47,49,53,51},{49,48,52,53},{46,47,51,50},{57,56,60,61},{53,52,56,57},{50,51,55,54},{52,50,54,56},{59,61,60,58},{54,55,59,58},{56,54,58,60},{55,57,61,59},{64,67,66,63},{62,65,77},{63,66,65,62},{66,69,68,65},{67,70,69,66},{65,68,77},{69,72,71,68},{70,73,72,69},{68,71,77},{73,76,75,72},{71,74,77},{72,75,74,71},{76,80,79,75},{74,78,77},{75,79,78,74},{79,82,81,78},{80,83,82,79},{78,81,77},{82,85,84,81},{83,86,85,82},{81,84,77},{86,64,63,85},{84,62,77},{85,63,62,84},{88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,87},},
	cols = {{228,194,0},{228,207,144},{199,196,191},{176,174,170},{0,152,176},},
	fcols = {1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,3,3,3,3,4,2,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,5},
}


-- math functions

	math.between = function(x, a, b)
		return x >= a and x < b
	end

	math.clamp = function(x, a, b)
		if math.between(x, a, b) then return x end
		if x < a then return a end
		if x >= b then return b end
	end

	math.bool2int = function(a)
		return a and 1 or 0
	end

	math.mod = function(x, a)
		return x - floor(x/a)*a
	end

	math.sign = function(x)
	    return x > 0 and 1 or -1
	end

	math.maprange = function(x, a1, b1, a2, b2)
		return (x - a1)/(b1 - a1)*(b2 - a2) + a2
	end

	math.isnan = function(x)
		return x ~= x
	end

	math.round = function(x)
		return math.floor(x + 0.5)
	end

	math.lerp = function(x, a, b)
		return (1-x)*a + x*b
	end

	math.isin = function(a, list)
		local flag = false
		for i = 1,#list do
			if list[i] == a then 
				flag = true 
				break
			end
		end
		return flag
	end

	math.randlist = function(list)
		local n = #list 
		local i = random(1, n)
		return list[i]
	end

	math.randprob = function(list, prob)
		local a = random()
		local cumsum = 0
		for i = 1,#list do
			if a <= prob[i] then return a end
			cumsum = cumsum + prob[i+1]
		end
	end

	math.rmdupe = function(list)
		local hash = {}
		local res = {}
		for _,v in ipairs(list) do
			if (not hash[v]) then
				res[#res+1] = v
				hash[v] = true
			end
		end
		return res 
	end

	math.sum = function(list)
		local tot = 0 
		for i = 1,#list do
			tot = tot + list[i]
		end
		return tot
	end

	math.maxf = function(list, fun)
		local v = list[1]
		local k = 1
		for i = 2,#list do
			if fun(v, list[i]) then
				v = list[i]
				k = i
			end
		end
		return v, k
	end

	-- verified (trust me 151022)
	math.isccw = function(x1, y1, x2, y2, x3, y3)
		local a1 = x2 - x1
		local a2 = y2 - y1
		local b1 = x3 - x1
		local b2 = y3 - y1
		return (a1*b2 - b1*a2) < 0
	end

	math.convexhull = function(points)
		local hull = {}
		-- min y-value 
		local p, k = math.maxf(points, function(u,v) return u[2] > v[2] end)
		for i = 1,#points do
			table.insert(hull, p)
			local q 
			-- q cant equal p
			for _,r in ipairs(points) do
				q = r 
				if q[1] ~= p[1] or q[2] ~= p[2] then break end 
			end
			-- find most ccw point
			for _,r in ipairs(points) do
				if math.isccw(p[1], p[2], q[1], q[2], r[1], r[2]) then
					q = r 
				end
			end
			if q == hull[1] then break end
			p = q 
		end
		return hull
	end

	math.polyb = function(points, col)
		for i = 1,#points-1 do
			line(points[i][1], points[i][2], points[i+1][1], points[i+1][2], 15)
		end
		line(points[1][1], points[1][2], points[#points][1], points[#points][2], 0)
	end

--

time = {

	t = 0,
	dt = 0,
	last_t = 0,
	count = 0,
	fps = 60,
	ts = 0,
	show = true,

	update = function(self)
		if keyp(20) then self.show = not self.show end
		local t = systime()
		self.dt = t - self.t
		self.t = t
		self.ts = self.t // 1000
		if t - self.last_t > 1000 then
			self.last_t = t
			self.fps = self.count
			self.count = 0
		else
			self.count = self.count + 1 
		end
	end,

	draw = function(self)
		if self.show then
			print(self.fps, 228, 1, 4, true)
		end
	end

}

mouse = {

	x = 120,
	y = 68,
	dx = 0,
	dy = 0,
	md = false,
	mp = false,
	mu = false,
	right = false,
	middle = false,
	scroll = 0,

	update = function(self)
		local mx, my, md, middle, right, scrollx, scrolly = sysmouse()
		self.dx = mx - self.x
		self.dy = my - self.y
		self.x = mx
		self.y = my
		self.mp = not self.md and md
		self.mu = self.md and not md
		self.md = md
		self.right = right
		self.middle = middle
		self.scroll = scrolly
	end,

	draw = function(self)
		line(0, self.y, WIDTH, self.y, 6)
		line(self.x, 0, self.x, HEIGHT, 6)
		print('(' .. self.x .. ',' .. self.y .. ')', self.x+2, self.y+2, 6)
		local w = print(self.x, 0, -6)
		print(self.x, self.x-w, 0, 6)
		print(self.y, 0, self.y-6 , 6)
	end

}

Vec = {

	new = function(x, y, z)
		local v = {x, y, z}
		setmetatable(v, Vec.mt)
		return v
	end,

	mt = {

		__add = function (u, v)
			return Vec.new(u[1]+v[1], u[2]+v[2], u[3]+v[3])
		end,

		__sub = function(u, v)
			return Vec.new(u[1]-v[1], u[2]-v[2], u[3]-v[3])
		end,

		__mul = function(k, v)
			if type(k) == "table" then
				return Vec.new(k[1]*v[1], k[2]*v[2], k[3]*v[3])
			else
				return Vec.new(k*v[1], k*v[2], k*v[3])
			end
		end,

		__div = function(v, k)
			if type(k) == "table" then
				return Vec.new(v[1]/k[1], v[2]/k[2], v[3]/k[3])
			else
				return Vec.new(v[1]/k, v[2]/k, v[3]/k)
			end
		end,

		__pow = function(v, k)
			return Vec.new(v[1]^k, v[2]^k, v[3]^k)
		end,

		__eq = function(u, v)
			return u[1] == v[1] and u[2] == v[2] and u[3] == v[3]
		end,

		__tostring = function(v)
			return "(" .. v[1] .. "," .. v[2] .. "," .. v[3] .. ")"
		end,

		__concat = function(s, v)
			return s .. "(" .. v[1] .. "," .. v[2] .. "," .. v[3] .. ")"
		end,

		__index = {

			normalise = function(self)
				local r = Vec.norm(self)
				self[1] = self[1] / r
				self[2] = self[2] / r
				self[3] = self[3] / r
				return self
			end,

			ortho_proj = function(self, camera)
				local x = Vec.dot(self, camera.x) + W2
				local y = Vec.dot(self, camera.y) - H2
				return x, -y
			end,

			pers_proj = function(self, camera)
				local x = Vec.dot(self, camera.x)
				local y = Vec.dot(self, camera.y)
				local r = Vec.dist(self, camera.o)
				x = camera.depth * x / r + W2
				y = camera.depth * y / r - H2
				return x, -y
			end,

			proj = function(self, camera)
				local x = Vec.dot(self, camera.x)
				local y = Vec.dot(self, camera.y)
				local r = Vec.dist(self, camera.o)
				x = camera.depth * x / r + W2
				y = camera.depth * y / r - H2
				return x, -y
			end,

			rotate = function(self, ax, ay, az)
				local cx = cos(ax)
				local sx = sin(ax)
				local cy = cos(ay)
				local sy = sin(ay)
				local cz = cos(az)
				local sz = sin(az)
				local x1 = self[1]
				local y1 = self[2] * cx - self[3] * sx
				local z1 = self[2] * sx + self[3] * cx
				local x2 = x1 * cy + z1 * sy
				local y2 = y1
				local z2 = -x1 * sy + z1 * cy
				self[1] = x2 * cz - y2 * sz
				self[2] = x2 * sz + y2 * cz
				self[3] = z2
				return self
			end,

			qrotate = function(self, theta, u)
				u = u / Vec.norm(u)
				local c = cos(theta/2)
				local s = sin(theta/2)
				local k = Vec.dot(s*u, self)
				local w = Vec.cross(s*u, self)
				local v = 2*k*s*u + (c*c-s*s)*self + 2*c*w
				self[1] = v[1]
				self[2] = v[2]
				self[3] = v[3]
				return self
			end,

			draw = function(self)
				local x, y = self:proj(camera)
				circb(x, y, 2, 4)
			end,
		}
	},

	norm = function(v)
		return sqrt(v[1]*v[1] + v[2]*v[2] + v[3]*v[3])
	end,

	dot = function(u, v)
		return u[1]*v[1] + u[2]*v[2] + u[3]*v[3]
	end,

	cross = function(u, v)
		return Vec.new(u[2]*v[3]-v[2]*u[3], v[1]*u[3]-u[1]*v[3], u[1]*v[2]-v[1]*u[2])
	end,

	avg = function(u)
		return (u[1] + u[2] + u[3]) / 3
	end,

	dist = function(u, v)
		return Vec.norm(u - v)
	end,

	mid = function(u, v)
		return (u + v) / 2
	end,

	round = function(u)
		return Vec.new(math.round(u[1]), math.round(u[2]), math.round(u[3]))
	end,

	line = function(x0, y0, u, v, camera, col)
		local x1, y1 = u:proj(camera)
		local x2, y2 = v:proj(camera)
		line(x1+x0, y1+y0, x2+x0, y2+y0, col)
	end,

	ysort = function(u, v)
		return u[2] < v[2]
	end

}

camera = {

	o = Vec.new(0, 0, 100),
	x = Vec.new(1, 0, 0),
	y = Vec.new(0, 1, 0),
	ax = 0,
	ay = 0,
	r = 100,
	depth = 100,

	load = function(self)
		self.o = Vec.new(0, 0, 200)
		self.ay = 3
		self.r = 200
		-- learping targets
		self.axt = -0.5
		self.ayt = 0.5
		self.rt = 100
		-- perspective projection
		Vec.mt.__index.proj = Vec.mt.__index.pers_proj
	end,

	update = function(self)
		-- update target
		if mouse.md then
			self.axt = math.clamp(self.axt - 0.03*mouse.dy, -PI/2, PI/2)
			self.ayt = self.ayt - 0.03*mouse.dx
		end
		self.rt = math.clamp(self.rt - 7*mouse.scroll, 60, 300)
		-- lerping
		self.ax = math.lerp(0.1, self.ax, self.axt)
		self.ay = math.lerp(0.1, self.ay, self.ayt)
		self.r = math.lerp(0.1, self.r, self.rt)
		-- update camera
		self.o = Vec.new(0, 0, self.r):rotate(self.ax, self.ay, 0)
		self.x = Vec.new(1, 0, 0):rotate(0, self.ay, 0)
		self.y = Vec.new(0, 1, 0):rotate(self.ax, self.ay, 0)

		-- reset camera (num 0)
		if keyp(27) then 
			self.axt = -0.5
			self.ayt = 0.5
			self.rt = 100
			Vec.mt.__index.proj = Vec.mt.__index.pers_proj
		end
		-- toggle perspective (num 5)
		if keyp(32) then 
			if Vec.mt.__index.proj == Vec.mt.__index.pers_proj then
				Vec.mt.__index.proj = Vec.mt.__index.ortho_proj
			else
				Vec.mt.__index.proj = Vec.mt.__index.pers_proj
			end
		end
	end,

}

color = {

	addr = 0x03FC0,

	new = function(r, g, b)
		local c = Vec.new(r, g, b)
		return c
	end,

	getcol = function(col)
		local addr = color.addr + 3*col
		local r = peek(addr)
		local g = peek(addr+1)
		local b = peek(addr+3)
		return Vec.new(r, g, b)
	end,

	setcol = function(c, col)
		local addr = color.addr + 3*col
		poke(addr, c[1])
		poke(addr+1, c[2])
		poke(addr+2, c[3])
	end,

}

Face = {

	new = function(verts, col)
		local f = {verts=verts, col=col}
		setmetatable(f, Face.mt)
		return f
	end,

	mt = {

		__index = {

			draw = function(self)
				local x0, y0 = table.unpack(self.verts[1].p)
				local x1, y1 = table.unpack(self.verts[2].p)
				local x2, y2
				for i = 3,#self.verts do
					x2, y2 = table.unpack(self.verts[i].p)
					tri(x0, y0, x1, y1, x2, y2, self.col)
					x1, y1 = x2, y2
				end
			end,

			drawWire = function(self)
				local x1, y1 = table.unpack(self.verts[#self.verts].p)
				local x2, y2 = table.unpack(self.verts[1].p)
				line(x1, y1, x2, y2, self.col)
				for i = 2,#self.verts do
					x1, y1 = x2, y2
					x2, y2 = table.unpack(self.verts[i].p)
					line(x1, y1, x2, y2, self.col)
				end
			end,

			seto = function(self)
				local m = self.verts[1].p[3]
				for i = 2,#self.verts do
					if m < self.verts[i].p[3] then
						m = self.verts[i].p[3]
					end
				end
				self.o = m
			end,

			isccw = function(self)
				local x1, y1 = table.unpack(self.verts[1].p)
				local x2, y2 = table.unpack(self.verts[2].p)
				local x3, y3 = table.unpack(self.verts[3].p)
				return math.isccw(x1, y1, x2, y2, x3, y3)
			end,

		}

	}

}

Mesh = {

	load = function(obj, scale)
		scale = scale or 30
		local mesh = {}
		-- load verticies
		mesh.verts = {}
		for i,v in ipairs(obj.verts) do
			mesh.verts[i] = scale * Vec.new(table.unpack(v))
		end
		-- load faces
		mesh.faces = {}
		for i,f in ipairs(obj.faces) do
			-- pointers to vertices 
			local vj = {}
			for _,j in ipairs(f) do
				table.insert(vj, mesh.verts[j])
			end
			mesh.faces[i] = Face.new(vj, obj.fcols[i]+7)
		end	
		setmetatable(mesh, Mesh.mt)
		-- color
		if obj.cols ~= nil then
			for i,c in ipairs(obj.cols) do
				color.setcol(c, 7+i)
			end
		end
		return mesh
	end,

	mt = {

		__index = {

			proj = function(self)
				for _,v in ipairs(self.verts) do
					local x, y = v:proj(camera)
					local r = Vec.dist(v, camera.o)
					v.p = Vec.new(x, y, r)
				end
			end,

			draw = function(self)
				-- project vertex
				for _,v in ipairs(self.verts) do
					local x, y = v:proj(camera)
					local r = Vec.dist(v, camera.o)
					v.p = Vec.new(x, y, r)
				end
				-- backface culling 
				local dfaces = {}
				for _,f in ipairs(self.faces) do
					if f:isccw() then table.insert(dfaces, f) end
				end
				-- compute depth, sort, and draw
				for _,f in ipairs(dfaces) do 
					f:seto() 
				end
				table.sort(dfaces, Mesh.zsort)
				for i,f in ipairs(dfaces) do
					f:draw()
				end
			end,

			draw_twosided = function(self)
				for _,v in ipairs(self.verts) do
					local x, y = v:proj(camera)
					local r = Vec.dist(v, camera.o)
					v.p = Vec.new(x, y, r)
				end
				-- compute depth, sort, and draw
				for _,f in ipairs(self.faces) do 
					f:seto() 
				end
				table.sort(self.faces, Mesh.zsort)
				for i,f in ipairs(self.faces) do
					f:draw()
				end
			end,

		}

	},

	zsort = function(u, v)
		return u.o > v.o
	end, 

}

debug = {

	log = {},
	show = true,

	update = function(self)
		self.log[1] = "vertices: " .. #um_mesh.verts
		self.log[2] = "faces: " .. #um_mesh.faces
		if keyp(44) then self.show = not self.show end
	end,

	draw = function(self)
		if not self.show then return end
		for i,v in ipairs(self.log) do
			print(v, 1, (i-1)*6+1, 15, 0, 1, 1)
		end
	end

}

-- TODO
-- point in polygon 
-- reflection 
-- rain particles
-- make scene class for multiple objects


camera:load()

scene = {

	bg_col = {177, 211, 208},
	meshes = {},

	load = function(self)
		color.setcol(self.bg_col, 7)
		self.meshes = {
			Mesh.load(Umbrella)
		}
	end,

	draw = function(self)
		-- background 
		cls(7)
		-- faces to draw
		local dfaces = {}
		for i,m in ipairs(self.meshes) do
			m:proj()
			-- backface culling 
			for _,f in ipairs(m.faces) do
				if f:isccw() then table.insert(dfaces, f) end
			end
		end
		-- compute depth, sort, and draw
		for _,f in ipairs(dfaces) do 
			f:seto() 
		end
		table.sort(dfaces, Mesh.zsort)
		for i,f in ipairs(dfaces) do
			f:draw()
		end
	end,

}

-- ico_mesh = Mesh.load(ico)
color.setcol({177, 211, 208}, 7)
um_mesh = Mesh.load(Umbrella)

function TIC()

	cls(7)

	mouse:update()
	camera:update()
	debug:update()

	-- draw stuff here
	um_mesh:draw()
	
	-- mouse:draw()
	time:draw()
	debug:draw()

	-- points = {}
	-- for _,v in ipairs(um_mesh.verts) do
	-- 	table.insert(points, v.p)
	-- end
	-- hull = math.convexhull(points)
	-- for i,v in ipairs(hull) do 
	-- 	print(i, v[1], v[2])
	-- end
	-- math.polyb(hull)

end

-- <PALETTE>
-- 000:1a1c2c5d275db13e53ef7d57ffcd75a7f07038b76425717929366f3b5dc941a6f673eff7f4f4f494b0c2566c86333c57
-- </PALETTE>

