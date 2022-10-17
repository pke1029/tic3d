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


Floor = {
	verts = {{-1.06,0,-1.36},{-1.5,-0,-0.85},{-1.71,-0,-0.21},{-1.66,-0,0.46},{-1.36,0,1.06},{-0.85,0,1.5},{-0.21,0,1.71},{0.46,0,1.66},{1.06,-0,1.36},{1.5,0,0.85},{1.71,0,0.21},{1.66,0,-0.46},{1.36,-0,-1.06},{0.85,-0,-1.5},{0.21,0,-1.71},{-0.46,0,-1.66},},
	faces = {{2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,1},},
	cols = {{0,152,176},},
	fcols = {1},
}
Inner = {
	verts = {{0.11,1.11,-0.76},{-0.25,0.92,-0.87},{-0.64,0.67,-0.9},{0.31,0.94,-0.8},{0.16,0.58,-0.95},{-0.04,0.16,-1.01},{0.54,0.9,-0.68},{0.63,0.49,-0.7},{0.66,0.02,-0.65},{0.67,1,-0.47},{0.89,0.7,-0.28},{1.05,0.34,-0.02},{0.62,1.2,-0.3},{0.79,1.1,0.07},{0.89,0.93,0.5},{0.4,1.21,-0.58},{0.42,1.37,-0.26},{0.38,1.45,0.15},{0.29,1.44,0.61},{0.19,1.41,-0.38},{-0.1,1.54,-0.1},{-0.41,1.58,0.25},{0.06,1.31,-0.59},{-0.36,1.32,-0.52},{-0.8,1.26,-0.38},},
	faces = {{3,6,5,2},{1,4,16},{2,5,4,1},{5,8,7,4},{6,9,8,5},{4,7,16},{8,11,10,7},{9,12,11,8},{7,10,16},{12,15,14,11},{10,13,16},{11,14,13,10},{15,19,18,14},{13,17,16},{14,18,17,13},{18,21,20,17},{19,22,21,18},{17,20,16},{21,24,23,20},{22,25,24,21},{20,23,16},{25,3,2,24},{23,1,16},{24,2,1,23},},
	cols = {{228,207,144},},
	fcols = {1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1},
}
Outter = {
	verts = {{0.11,1.11,-0.76},{-0.25,0.92,-0.87},{-0.64,0.67,-0.9},{0.31,0.94,-0.8},{0.16,0.58,-0.95},{-0.04,0.16,-1.01},{0.54,0.9,-0.68},{0.63,0.49,-0.7},{0.66,0.02,-0.65},{0.67,1,-0.47},{0.89,0.7,-0.28},{1.05,0.34,-0.02},{0.62,1.2,-0.3},{0.79,1.1,0.07},{0.89,0.93,0.5},{0.4,1.21,-0.58},{0.42,1.37,-0.26},{0.38,1.45,0.15},{0.29,1.44,0.61},{0.19,1.41,-0.38},{-0.1,1.54,-0.1},{-0.41,1.58,0.25},{0.06,1.31,-0.59},{-0.36,1.32,-0.52},{-0.8,1.26,-0.38},},
	faces = {{3,2,5,6},{1,16,4},{2,1,4,5},{5,4,7,8},{6,5,8,9},{4,16,7},{8,7,10,11},{9,8,11,12},{7,16,10},{12,11,14,15},{10,16,13},{11,10,13,14},{15,14,18,19},{13,16,17},{14,13,17,18},{18,17,20,21},{19,18,21,22},{17,16,20},{21,20,23,24},{22,21,24,25},{20,16,23},{25,24,2,3},{23,16,1},{24,23,1,2},},
	cols = {{228,194,0},},
	fcols = {1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1},
}
Stick = {
	verts = {{-0.27,0.22,0.36},{0.38,1.18,-0.53},{-0.29,0.21,0.35},{0.36,1.18,-0.55},{-0.26,0.2,0.35},{0.39,1.16,-0.54},{-0.27,0.2,0.34},{0.37,1.16,-0.56},},
	faces = {{1,2,4,3},{3,4,8,7},{7,8,6,5},{5,6,2,1},},
	cols = {{199,196,191},},
	fcols = {1,1,1,1},
}
Handle = {
	verts = {{-0.27,0.23,0.38},{-0.3,0.22,0.35},{-0.24,0.19,0.36},{-0.28,0.19,0.33},{-0.3,0.18,0.42},{-0.33,0.18,0.39},{-0.27,0.15,0.4},{-0.31,0.14,0.37},{-0.33,0.13,0.48},{-0.37,0.1,0.48},{-0.3,0.09,0.46},{-0.34,0.07,0.46},{-0.3,0.1,0.56},{-0.33,0.07,0.58},{-0.28,0.06,0.54},{-0.3,0.03,0.56},{-0.24,0.11,0.62},{-0.23,0.09,0.66},{-0.21,0.07,0.6},{-0.21,0.05,0.64},{-0.16,0.16,0.62},{-0.13,0.16,0.66},{-0.14,0.12,0.6},{-0.11,0.12,0.64},{-0.12,0.22,0.58},{-0.08,0.23,0.59},{-0.09,0.18,0.56},{-0.05,0.2,0.57},},
	faces = {{4,3,7,8},{7,5,9,11},{1,2,6,5},{3,1,5,7},{2,4,8,6},{12,11,15,16},{6,8,12,10},{8,7,11,12},{5,6,10,9},{15,13,17,19},{9,10,14,13},{11,9,13,15},{10,12,16,14},{18,20,24,22},{14,16,20,18},{16,15,19,20},{13,14,18,17},{24,23,27,28},{20,19,23,24},{17,18,22,21},{19,17,21,23},{26,28,27,25},{21,22,26,25},{23,21,25,27},{22,24,28,26},{2,1,3,4},},
	cols = {{176,174,170},},
	fcols = {1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1},
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
--

-- higher level functions

	convexhull = function(points)
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

	polyb = function(points, col)
		for i = 1,#points-1 do
			line(points[i][1], points[i][2], points[i+1][1], points[i+1][2], 15)
		end
		line(points[1][1], points[1][2], points[#points][1], points[#points][2], 0)
	end

	point_in_hull = function(p, poly)
		for i = 1,#poly-1 do
			if math.isccw(p[1], p[2], poly[i][1], poly[i][2], poly[i+1][1], poly[i+1][2]) then
				return false
			end
		end
		return not math.isccw(p[1], p[2], poly[#poly][1], poly[#poly][2], poly[1][1], poly[1][2])
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

			-- accurate
			pers_proj = function(self, camera)
				local x = Vec.dot(self, camera.x)
				local y = Vec.dot(self, camera.y)
				local r = Vec.dist(self, camera.o)
				local r2 = sqrt(r^2 - x^2 - y^2)
				x = camera.depth * x / r2 + W2
				y = camera.depth * y / r2 - H2
				return x, -y
			end,

			-- simple proj
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

			proj = function(self)
				for _,v in ipairs(self.verts) do
					-- v.p = (x, y, r)
					v.p[1], v.p[2] = v:proj(camera)
					v.p[3] = Vec.dist(v, camera.o)
				end
			end

		}

	}

}

Tface = {

	new = function(verts, uv)
		local f = {verts=verts, uv=uv}
		setmetatable(f, Tface.mt)
		return f
	end,

	mt = {

		__index = {

			draw = function(self)
			local x0, y0, z0 = table.unpack(self.verts[1].p)
			local u0, v0 = self.uv[1], self.uv[2]
			local x1, y1, z1 = table.unpack(self.verts[2].p)
			local u1, v1 = self.uv[3], self.uv[4]
			local x2, y2, z2, u2, v2
			for i = 3,#self.verts do
				x2, y2, z2 = table.unpack(self.verts[i].p)
				u2, v2 = self.uv[2*i-1], self.uv[2*i]
				ttri(x0, y0, x1, y1, x2, y2, u0, v0, u1, v1, u2, v2, 0, 0, z0, z1, z2)
				x1, y1, z1 = x2, y2, z2
				u1, v1 = u2, v2
			end
		end,

		seto = Face.mt.__index.seto,
		isccw = Face.mt.__index.isccw,
		proj = Face.mt.__index.proj,

		}

	}

}

Mesh = {

	cols = {},

	load = function(obj, scale)
		scale = scale or 30
		local mesh = {}
		-- load verticies
		mesh.verts = {}
		for i,v in ipairs(obj.verts) do
			mesh.verts[i] = scale * Vec.new(table.unpack(v))
			mesh.verts[i].p = Vec.new(0, 0, 0)
		end
		-- load faces
		mesh.faces = {}
		for i,f in ipairs(obj.faces) do
			-- pointers to vertices 
			local vj = {}
			for _,j in ipairs(f) do
				table.insert(vj, mesh.verts[j])
			end
			mesh.faces[i] = Face.new(vj, #Mesh.cols+obj.fcols[i]+7)
		end	
		setmetatable(mesh, Mesh.mt)
		-- color
		if obj.cols ~= nil then
			for i,c in ipairs(obj.cols) do
				color.setcol(c, #Mesh.cols+7+i)
				table.insert(Mesh.cols, c)
			end
		end
		return mesh
	end,

	mt = {

		__index = {

			proj = function(self)
				for _,v in ipairs(self.verts) do
					-- v.p = (x, y, r)
					v.p[1], v.p[2] = v:proj(camera)
					v.p[3] = Vec.dist(v, camera.o)
				end
			end,

			draw = function(self)
				-- project vertex
				self:proj()
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
				self:proj()
				-- compute depth, sort, and draw
				for _,f in ipairs(self.faces) do 
					f:seto() 
				end
				table.sort(self.faces, Mesh.zsort)
				for i,f in ipairs(self.faces) do
					f:draw()
				end
			end,

			get_proj = function(self)
				local s = {}
				for i,v in ipairs(self.verts) do
					s[i] = v.p 
				end
				return s 
			end

		}

	},

	zsort = function(u, v)
		return u.o > v.o
	end, 

}


-- rain = {

	-- 	col = {149, 198, 198},
	-- 	particles = {},
	-- 	maxlife = 20,
	-- 	minlife = 10,
	-- 	minradius = 5,
	-- 	maxradius = 10,
	-- 	next = 0,
	-- 	t = 0,

	-- 	load = function(self)
	-- 		color.setcol(self.col, 6)
	-- 		table.insert(self.particles, {o = Vec.new(0,0,0), r = 1.7*30, fac=1})
	-- 	end,

	-- 	update = function(self)
	-- 		self:emittimmer()
	-- 		-- update particles
	-- 		for i,p in ipairs(self.particles) do
	-- 			p.fac = (time.t - p.start) / p.lifespan
	-- 			-- remove
	-- 			if p.fac > 1 then
	-- 				table.remove(self.particles, i)
	-- 			end
	-- 		end
	-- 		self.t = self.t + 1
	-- 	end,

	-- 	emittimmer = function(self)
	-- 		if self.next <= self.t then
	-- 			self:emit()
	-- 			self.next = self.next + 3
	-- 		end
	-- 	end,

	-- 	emit = function(self)
	-- 		local p = {}
	-- 		local r = random() * 50 + 30
	-- 		local a = random() * 2*PI
	-- 		p.o = Vec.new(r*sin(a), 0, r*cos(a)) 
	-- 		p.fac = 0 
	-- 		p.r = random() * (self.maxradius - self.minradius) + self.minradius
	-- 		p.start = time.t 
	-- 		p.lifespan = 400
	-- 		table.insert(self.particles, p)
	-- 	end,

	-- 	draw = function(self)
	-- 		for _,p in ipairs(self.particles) do
	-- 			if  Vec.mt.__index.proj ~= Vec.mt.__index.ortho_proj then
	-- 				local r = p.r * p.fac 
	-- 				local x0, y0 = p.o:proj(camera) 
	-- 				local d = Vec.dist(camera.o, p.o)
	-- 				local a, _ = (r * camera.x):proj(camera) - W2
	-- 				-- local a = (x2-x1)/2
	-- 				local v = Vec.new(camera.y[1], 0, camera.y[3])
	-- 				v:normalise()
	-- 				local _, y3 = (p.o + r * v):proj(camera)
	-- 				local _, y4 = (p.o - r * v):proj(camera)
	-- 				local b = (y4-y3) / 2
	-- 				ellib(x0, (y4+y3)/2, a, b, 6)
	-- 			else
	-- 				local x, y = p.o:proj(camera) 
	-- 				local d = Vec.dist(camera.o, p.o)
	-- 				local s = camera.o[2] / d 
	-- 				local r = p.r * p.fac 
	-- 				ellib(x, y, r, r*s, 6)
	-- 			end
	-- 		end
	-- 	end,

-- }

rain = {

	col = {149, 198, 198},
	particles = {},
	-- uv = {30+32, 0, 0+32, 0, 0+32, 30, 30+32, 30},
	uv = {30+64, 0+96, 0+64, 0+96, 0+64, 30+96, 30+64, 30+96},
	minradius = 5,
	maxradius = 10,
	next = 0,
	t = 0,

	load = function(self)
		color.setcol(self.col, 6)
		for i = 1,20 do self:emit() end
	end,

	update = function(self)
		self:emittimmer()
		self.t = self.t + 1
		-- update particles
		for i,p in ipairs(self.particles) do
			p.t = p.t + 1
			if p.t > 28 then 
				table.remove(self.particles, i)
			end
			local i = floor(p.t/2) % 4
			local j = floor(p.t/2) // 4
			p.f.uv = {30+i*32, 0+j*32, 0+i*32, 0+j*32, 0+i*32, 30+j*32, 30+i*32, 30+j*32}
		end
	end,

	emittimmer = function(self)
		if self.next <= self.t then
			self:emit()
			self.next = self.next + 3
		end
	end,

	emit = function(self)
		local p = {}
		local r = random() * 50 + 30
		local a = random() * 2*PI
		p.o = Vec.new(r*sin(a), 0, r*cos(a)) 
		p.t = 0
		p.r = random() * (self.maxradius - self.minradius) + self.minradius
		p.f = self.new_face(p.o, p.r)
		table.insert(self.particles, p)
	end,

	new_face = function(o, r)
		local verts = {{r,0,r}, {-r,0,r}, {-r,0,-r}, {r,0,-r}}
		for i,v in ipairs(verts) do
			verts[i] = o + v
			verts[i].p = Vec.new(0,0,0)
		end
		local f = Tface.new(verts, rain.uv)
		return f 
	end,

	proj = function(self)
		for _,p in ipairs(self.particles) do
			p.f:proj()
		end
	end

}


debug = {

	log = {},
	show = true,

	update = function(self)
		-- self.log[1] = "vertices: " .. #um_mesh.verts
		-- self.log[2] = "faces: " .. #um_mesh.faces
		if keyp(44) then self.show = not self.show end
	end,

	draw = function(self)
		if not self.show then return end
		for i,v in ipairs(self.log) do
			print(v, 1, (i-1)*6+1, 15, 0, 1, 1)
		end
	end

}

scene = {

	bg_col = {177, 211, 208},
	meshes = {},

	load = function(self)
		color.setcol(self.bg_col, 7)
		self.meshes = {
			Mesh.load(Outter),
			Mesh.load(Inner),
			Mesh.load(Floor),
			Mesh.load(Handle),
			Mesh.load(Stick)
		}
		self.um = self.meshes[1]:get_proj()
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
		-- particle system
		rain:proj()
		for i,p in ipairs(rain.particles) do
			table.insert(dfaces, p.f)
		end
		-- compute depth, sort, and draw
		for _,f in ipairs(dfaces) do 
			f:seto() 
		end
		table.sort(dfaces, Mesh.zsort)
		for i,f in ipairs(dfaces) do
			f:draw()
		end
		-- draw convex hull
		self.hull = convexhull(self.um)
		if point_in_hull({mouse.x, mouse.y}, self.hull) then 
			polyb(self.hull)
		end
	end,

}

-- TODO
-- reflection 
-- rain particles
-- raycast + ray intersection 
-- word cast 


camera:load()
scene:load()
rain:load()


function TIC()

	cls(7)

	mouse:update()
	time:update()
	camera:update()
	rain:update()
	debug:update()

	scene:draw()
	-- rain:draw()
	
	-- mouse:draw()
	time:draw()
	debug:draw()

end

-- <TILES>
-- 017:0000000000000000000000000000000000000000000000000000006000000000
-- 021:0000000000000000000000000000000000000000000000600000060600000060
-- 025:0000000000000000000000000000000000000666000060000000600000006000
-- 026:0000000000000000000000000000000000000000600000006000000060000000
-- 029:0000000000000000000000000000066600006000000600000006000000060000
-- 030:0000000000000000000000000000000060000000060000000600000006000000
-- 041:0000066600000000000000000000000000000000000000000000000000000000
-- 045:0000600000000666000000000000000000000000000000000000000000000000
-- 046:6000000000000000000000000000000000000000000000000000000000000000
-- 077:0000000000000000000000000000000000000000000000000000000000006666
-- 078:0000000000000000000000000000000000000000000000000000000060000000
-- 081:0000000000000000000066660006000000600000006000000060000000600000
-- 082:0000000000000000600000000600000000600000006000000060000000600000
-- 085:0000000000006666000600000060000006000000060000000600000006000000
-- 086:0000000060000000060000000060000000060000000600000006000000060000
-- 089:0000666600660000060000000600000060000000600000006000000060000000
-- 090:6000000006600000000600000006000000006000000060000000600000006000
-- 092:0000000000000000000000000000000000000006000000060000000600000006
-- 093:0066000006000000600000006000000000000000000000000000000000000000
-- 094:0660000000060000000060000000600000000600000006000000060000000600
-- 097:0060000000060000000066660000000000000000000000000000000000000000
-- 098:0060000006000000600000000000000000000000000000000000000000000000
-- 101:0600000000600000000600000000666600000000000000000000000000000000
-- 102:0006000000600000060000006000000000000000000000000000000000000000
-- 105:6000000006000000060000000066000000006666000000000000000000000000
-- 106:0000600000060000000600000660000060000000000000000000000000000000
-- 108:0000000600000000000000000000000000000000000000000000000000000000
-- 109:0000000060000000600000000600000000660000000066660000000000000000
-- 110:0000060000006000000060000006000006600000600000000000000000000000
-- 129:0000000000000000000000000000000000000000000000000000666600660000
-- 130:0000000000000000000000000000000000000000000000006000000006600000
-- 133:0000000000000000000000000000000000000000000666660660000060000000
-- 134:0000000000000000000000000000000000000000660000000066000000006000
-- 136:0000000000000000000000000000000000000000000000000000000000000006
-- 137:0000000000000000000000000000000000066666066000006000000000000000
-- 138:0000000000000000000000000000000066000000006600000000600000000600
-- 140:0000000000000000000000000000000000000000000000060000006000000600
-- 141:0000000000000000000000000006666606600000600000000000000000000000
-- 142:0000000000000000000000006600000000660000000066000000006000000006
-- 144:0000000000000000000000060000000600000060000000600000006000000060
-- 145:0600000060000000000000000000000000000000000000000000000000000000
-- 146:0006000000006000000006000000060000000060000000600000006000000060
-- 148:0000000600000060000000600000060000000600000006000000060000000600
-- 150:0000060000000060000000600000000600000006000000060000000600000006
-- 152:0000006000000600000006000000600000006000000060000000600000006000
-- 154:0000006000000006000000060000000000000000000000000000000000000000
-- 155:0000000000000000000000006000000060000000600000006000000060000000
-- 156:0000060000006000000060000006000000060000000600000006000000060000
-- 158:0000000600000000000000000000000000000000000000000000000000000000
-- 159:0000000060000000600000000600000006000000060000000600000006000000
-- 160:0000006000000006000000060000000000000000000000000000000000000000
-- 161:0000000000000000000000006000000006000000006600000000666600000000
-- 162:0000006000000600000006000000600000060000066000006000000000000000
-- 164:0000060000000600000000600000006000000006000000000000000000000000
-- 165:0000000000000000000000000000000000000000600000000660000000066666
-- 166:0000000600000006000000600000006000000600000060000066000066000000
-- 168:0000600000006000000006000000060000000060000000060000000000000000
-- 169:0000000000000000000000000000000000000000000000006000000006600000
-- 170:0000000000000000000000060000000600000060000006000000600000660000
-- 171:6000000060000000000000000000000000000000000000000000000000000000
-- 172:0006000000060000000060000000600000000600000006000000006000000006
-- 173:0000000000000000000000000000000000000000000000000000000060000000
-- 174:0000000000000000000000000000000000000006000000060000006000006600
-- 175:0600000006000000600000006000000000000000000000000000000000000000
-- 185:0006666600000000000000000000000000000000000000000000000000000000
-- 186:6600000000000000000000000000000000000000000000000000000000000000
-- 189:0660000000066666000000000000000000000000000000000000000000000000
-- 190:0066000066000000000000000000000000000000000000000000000000000000
-- 192:0000000000000000000000000000000000000006000000600000060000006000
-- 193:0000000000000000000666660660000060000000000000000000000000000000
-- 194:0000000000000000660000000066000000006600000000600000000600000000
-- 195:0000000000000000000000000000000000000000000000000000000060000000
-- 196:0000000000000000000000000000000600000060000006000000600000060000
-- 197:0000000000066666066000006000000000000000000000000000000000000000
-- 198:0000000066000000006600000000660000000060000000060000000000000000
-- 199:0000000000000000000000000000000000000000000000006000000006000000
-- 200:0000000000000000000000660000060000006000000600000060000000600000
-- 201:0006666666600000000000000000000000000000000000000000000000000000
-- 202:6600000000666000000006600000000600000000000000000000000000000000
-- 203:0000000000000000000000000000000060000000060000000060000000600000
-- 208:0000600000060000000600000060000000600000006000000060000000600000
-- 211:6000000006000000060000000060000000600000006000000060000000600000
-- 212:0006000000600000006000000600000006000000060000000600000006000000
-- 215:0600000000600000006000000006000000060000000600000006000000060000
-- 216:0600000006000000060000006000000060000000600000006000000060000000
-- 219:0006000000060000000600000000600000006000000060000000600000006000
-- 224:0060000000600000000600000006000000006000000060000000060000000060
-- 226:0000000000000000000000000000000000000000000000000000000600000060
-- 227:0060000000600000060000000600000060000000600000000000000000000000
-- 228:0600000006000000006000000060000000060000000600000000600000000600
-- 230:0000000000000000000000000000000000000000000000000000000000000006
-- 231:0006000000060000006000000060000006000000060000006000000000000000
-- 232:6000000060000000060000000600000006000000006000000060000000060000
-- 235:0000600000006000000600000006000000060000006000000060000006000000
-- 240:0000000600000000000000000000000000000000000000000000000000000000
-- 241:6000000006600000000666660000000000000000000000000000000000000000
-- 242:0000660000660000660000000000000000000000000000000000000000000000
-- 244:0000006000000006000000000000000000000000000000000000000000000000
-- 245:0000000060000000066000000006666600000000000000000000000000000000
-- 246:0000006000006600006600006600000000000000000000000000000000000000
-- 248:0000600000000600000000660000000000000000000000000000000000000000
-- 249:0000000000000000000000006660000000066666000000000000000000000000
-- 250:0000000000000006000006600066600066000000000000000000000000000000
-- 251:6000000000000000000000000000000000000000000000000000000000000000
-- </TILES>

-- <PALETTE>
-- 000:1a1c2c5d275db13e53ef7d57ffcd75a7f07038b76425717929366f3b5dc941a6f673eff7f4f4f494b0c2566c86333c57
-- </PALETTE>

