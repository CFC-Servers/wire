--------------------------------------------------------------------------------
--  Vector support                                                            --
--------------------------------------------------------------------------------

local delta  = wire_expression2_delta

local random = math.random
local Vector = Vector
local sqrt = math.sqrt
local floor = math.floor
local ceil = math.ceil
local pi = math.pi
local atan2 = math.atan2
local asin = math.asin
local rad2deg = 180 / pi
local deg2rad = pi / 180

-- TODO: add reflect?
-- TODO: add absdotproduct?
-- TODO: add helper for angle and dotproduct? (just strange?)

--------------------------------------------------------------------------------

registerType("vector", "v", { 0, 0, 0 },
	nil,
	function(self, output) return Vector(rawget(output, 1), rawget(output, 2), rawget(output, 3)) end,
	function(retval)
		if isvector(retval) then return end
		if not istable(retval) then error("Return value is neither a Vector nor a table, but a "..type(retval).."!",0) end
		if #retval ~= 3 then error("Return value does not have exactly 3 entries!",0) end
	end,
	function(v)
		return not isvector(v) and (not istable(v) or #v ~= 3)
	end
)

--------------------------------------------------------------------------------

__e2setcost(1) -- approximated

e2function vector vec()
	return { 0, 0, 0 }
end

__e2setcost(2)

e2function vector vec(x)
	return { x, x, x }
end

e2function vector vec(x, y, z)
	return { x, y, z }
end

e2function vector vec(vector2 v2)
	return { rawget(v2, 1), rawget(v2, 2), 0 }
end

e2function vector vec(vector2 v2, z)
	return { rawget(v2, 1), rawget(v2, 2), z }
end

e2function vector vec(vector4 v4)
	return { rawget(v4, 1), rawget(v4, 2), rawget(v4, 3) }
end

--- Convert Angle -> Vector
e2function vector vec(angle ang)
	return { rawget(ang, 1), rawget(ang, 2), rawget(ang, 3) }
end

--------------------------------------------------------------------------------

registerOperator("ass", "v", "v", function(self, args)
	local op1, op2, scope = rawget(args, 2), rawget(args, 3), rawget(args, 4)

	local      rv2 = rawget(op2, 1)(self, op2)
	local scopes = rawget(self, "Scopes")
	local scopesScope = rawget(scopes, scope)
	rawset(scopesScope, op1, rv2)

	local vclk = rawget(scopesScope, "vclk")
	rawset(vclk, op1, true)

	return rv2
end)

--------------------------------------------------------------------------------

e2function number vector:operator_is()
	if rawget(this, 1) > delta or -rawget(this, 1) > delta or
	   rawget(this, 2) > delta or -rawget(this, 2) > delta or
	   rawget(this, 3) > delta or -rawget(this, 3) > delta
	   then return 1 else return 0 end
end

e2function number vector:operator==( vector other )
	if rawget(this, 1) - rawget(other, 1) <= delta and rawget(other, 1) - rawget(this, 1) <= delta and
	   rawget(this, 2) - rawget(other, 2) <= delta and rawget(other, 2) - rawget(this, 2) <= delta and
	   rawget(this, 3) - rawget(other, 3) <= delta and rawget(other, 3) - rawget(this, 3) <= delta
	   then return 1 else return 0 end
end

e2function number vector:operator!=( vector other )
	if rawget(this, 1) - rawget(other, 1) > delta or rawget(other, 1) - rawget(this, 1) > delta or
	   rawget(this, 2) - rawget(other, 2) > delta or rawget(other, 2) - rawget(this, 2) > delta or
	   rawget(this, 3) - rawget(other, 3) > delta or rawget(other, 3) - rawget(this, 3) > delta
	   then return 1 else return 0 end
end

--------------------------------------------------------------------------------

e2function vector vector:operator_neg()
	return { -rawget(this, 1), -rawget(this, 2), -rawget(this, 3) }
end

e2function vector operator+(lhs, vector rhs)
	return { lhs + rawget(rhs, 1), lhs + rawget(rhs, 2), lhs + rawget(rhs, 3) }
end

e2function vector operator+(vector lhs, rhs)
	return { rawget(lhs, 1) + rhs, rawget(lhs, 2) + rhs, rawget(lhs, 3) + rhs }
end

e2function vector operator+(vector lhs, vector rhs)
	return { rawget(lhs, 1) + rawget(rhs, 1), rawget(lhs, 2) + rawget(rhs, 2), rawget(lhs, 3) + rawget(rhs, 3) }
end

e2function vector operator-(lhs, vector rhs)
	return { lhs - rawget(rhs, 1), lhs - rawget(rhs, 2), lhs - rawget(rhs, 3) }
end

e2function vector operator-(vector lhs, rhs)
	return { rawget(lhs, 1) - rhs, rawget(lhs, 2) - rhs, rawget(lhs, 3) - rhs }
end

e2function vector operator-(vector lhs, vector rhs)
	return { rawget(lhs, 1) - rawget(rhs, 1), rawget(lhs, 2) - rawget(rhs, 2), rawget(lhs, 3) - rawget(rhs, 3) }
end

e2function vector operator*(lhs, vector rhs)
	return { lhs * rawget(rhs, 1), lhs * rawget(rhs, 2), lhs * rawget(rhs, 3) }
end

e2function vector operator*(vector lhs, rhs)
	return { rawget(lhs, 1) * rhs, rawget(lhs, 2) * rhs, rawget(lhs, 3) * rhs }
end

e2function vector operator*(vector lhs, vector rhs)
	return { rawget(lhs, 1) * rawget(rhs, 1), rawget(lhs, 2) * rawget(rhs, 2), rawget(lhs, 3) * rawget(rhs, 3) }
end

e2function vector operator/(lhs, vector rhs)
	return { lhs / rawget(rhs, 1), lhs / rawget(rhs, 2), lhs / rawget(rhs, 3) }
end

e2function vector operator/(vector lhs, rhs)
	return { rawget(lhs, 1) / rhs, rawget(lhs, 2) / rhs, rawget(lhs, 3) / rhs }
end

e2function vector operator/(vector lhs, vector rhs)
	return { rawget(lhs, 1) / rawget(rhs, 1), rawget(lhs, 2) / rawget(rhs, 2), rawget(lhs, 3) / rawget(rhs, 3) }
end

e2function number vector:operator[](index)
	return this[floor(rawget(math, "Clamp")(index, 1, 3) + 0.5)]
end

e2function number vector:operator[](index, value)
	this[ floor(rawget(math, "Clamp")(index, 1, 3) + 0.5) ] = value
	return value
end

--------------------------------------------------------------------------------

__e2setcost(10) -- temporary

--- Returns a uniformly distributed, random, normalized direction vector.
e2function vector randvec()
	local s,a, x,y

	--[[
	  This is a variant of the algorithm for computing a random point
	  on the unit sphere; the algorithm is suggested in Knuth, v2,
	  3rd ed, p136; and attributed to Robert E Knop, CACM, 13 (1970),
	  326.
	]]
	-- translated to lua from http://mhda.asiaa.sinica.edu.tw/mhda/apps/gsl-1.6/randist/sphere.c

	-- Begin with the polar method for getting x,y inside a unit circle
	repeat
		x = random() * 2 - 1
		y = random() * 2 - 1
		s = x*x + y*y
	until s <= 1.0

	a = 2 * sqrt(1 - s) -- factor to adjust x,y so that x^2+y^2 is equal to 1-z^2
	return Vector(x*a, y*a, s * 2 - 1) -- z uniformly distributed from -1 to 1

	--[[
	-- This variant saves 2 multiplications per loop, woo. But it's not readable and not verified, thus commented out.
	-- I will also not add a cheaper non-uniform variant, as that can easily be derived from the other randvec functions and V:normalize().
	-- Begin with the polar method for getting x,y inside a (strangely skewed) unit circle
	repeat
		x = random()
		y = random()
		s = x*(x-1) + y*(y-1)
	until s <= -0.25

	a = sqrt(-16 - s*64) -- factor to adjust x,y so that x^2+y^2 is equal to 1-z^2
	return Vector((x-0.5)*a, (y-0.5)*a, s * 8 + 3) -- z uniformly distributed from -1 to 1
	]]
end

__e2setcost(5)

--- Returns a random vector with its components between <min> and <max>
e2function vector randvec( normal min, normal max)
	local range = max-min
	return Vector(min+random()*range, min+random()*range, min+random()*range)
end

--- Returns a random vector between <min> and <max>
e2function vector randvec(vector min, vector max)
	local minx, miny, minz = rawget(min, 1), rawget(min, 2), rawget(min, 3)
	return Vector(minx+random()*(rawget(max, 1)-minx), miny+random()*(rawget(max, 2)-miny), minz+random()*(rawget(max, 3)-minz))
end

--------------------------------------------------------------------------------

__e2setcost(5)

e2function number vector:length()
    local this1 = rawget(this, 1)
    local this2 = rawget(this, 2)
    local this3 = rawget(this, 3)
    return ((this1 * this1) + (this2 * this2) + (this3 * this3)) ^ 0.5
end

e2function number vector:length2()
    local this1 = rawget(this, 1)
    local this2 = rawget(this, 2)
    local this3 = rawget(this, 3)
    return (this1 * this1) + (this2 * this2) + (this3 * this3)
end

e2function number vector:distance(vector other)
	local dx, dy, dz = rawget(this, 1) - rawget(other, 1), rawget(this, 2) - rawget(other, 2), rawget(this, 3) - rawget(other, 3)
	return (dx * dx + dy * dy + dz * dz) ^ 0.5
end

e2function number vector:distance2( vector other )
	local dx, dy, dz = rawget(this, 1) - rawget(other, 1), rawget(this, 2) - rawget(other, 2), rawget(this, 3) - rawget(other, 3)
	return dx * dx + dy * dy + dz * dz
end

e2function vector vector:normalized()
    local this1 = rawget(this, 1)
    local this2 = rawget(this, 2)
    local this3 = rawget(this, 3)
	local len = ((this1 * this1) + (this2 * this2) + (this3 * this3)) ^ 0.5

	if len > delta then
		return { this1 / len, this2 / len, this3 / len }
	else
		return { 0, 0, 0 }
	end
end

e2function number vector:dot( vector other )
	return rawget(this, 1) * rawget(other, 1) + rawget(this, 2) * rawget(other, 2) + rawget(this, 3) * rawget(other, 3)
end

e2function vector vector:cross( vector other )
    local this1 = rawget(this, 1)
    local this2 = rawget(this, 2)
    local this3 = rawget(this, 3)

    local other1 = rawget(other, 1)
    local other2 = rawget(other, 2)
    local other3 = rawget(other, 3)

	return {
		this2 * other3 - this3 * other2,
		this3 * other1 - this1 * other3,
		this1 * other2 - this2 * other1
	}
end

__e2setcost(10)

--- returns the outer product (tensor product) of two vectors
e2function matrix vector:outerProduct( vector other )
    local this1 = rawget(this, 1)
    local this2 = rawget(this, 2)
    local this3 = rawget(this, 3)

    local other1 = rawget(other, 1)
    local other2 = rawget(other, 2)
    local other3 = rawget(other, 3)

	return {
		this1 * this1, this1 * other2, this1 * other3,
		this2 * this1, this2 * other2, this2 * other3,
		this3 * this1, this3 * other2, this3 * other3,
	}
end

__e2setcost(15)
e2function vector vector:rotateAroundAxis(vector axis, degrees)
    local this1 = rawget(this, 1)
    local this2 = rawget(this, 2)
    local this3 = rawget(this, 3)

	local ca, sa = mathCos(degrees*deg2rad), mathSin(degrees*deg2rad)
	local x,y,z = rawget(axis, 1), rawget(axis, 2), rawget(axis, 3)
	local length = (x*x+y*y+z*z)^0.5
	x,y,z = x/length, y/length, z/length

	return {(ca + (x^2)*(1-ca)) * this1 + (x*y*(1-ca) - z*sa) * this2 + (x*z*(1-ca) + y*sa) * this3,
			(y*x*(1-ca) + z*sa) * this1 + (ca + (y^2)*(1-ca)) * this2 + (y*z*(1-ca) - x*sa) * this3,
			(z*x*(1-ca) - y*sa) * this1 + (z*y*(1-ca) + x*sa) * this2 + (ca + (z^2)*(1-ca)) * this3}
end

__e2setcost(5)

e2function vector vector:rotate( angle ang )
	local v = Vector(rawget(this, 1), rawget(this, 2), rawget(this, 3))
	v:Rotate(Angle(rawget(ang, 1), rawget(ang, 2), rawget(ang, 3)))
	return v
end

e2function vector vector:rotate( normal pitch, normal yaw, normal roll )
	local v = Vector(rawget(this, 1), rawget(this, 2), rawget(this, 3))
	v:Rotate(Angle(pitch, yaw, roll))
	return v
end

e2function vector2 vector:dehomogenized()
    local this1 = rawget(this, 1)
    local this2 = rawget(this, 2)

	local w = rawget(this, 3)
	if w == 0 then return { this1, this2 } end
	return { this1/w, this2/w }
end

e2function vector positive(vector rv1)
    local rv1_1 = rawget(rv1, 1)
    local rv1_2 = rawget(rv1, 2)
    local rv1_3 = rawget(rv1, 3)

	return {
		rv1_1 >= 0 and rv1_1 or -rv1_1,
		rv1_2 >= 0 and rv1_2 or -rv1_2,
		rv1_3 >= 0 and rv1_3 or -rv1_3,
	}
end

__e2setcost(3)

--- Convert the magnitude of the vector to radians
e2function vector toRad(vector rv1)
	return Vector(rawget(rv1, 1) * deg2rad, rawget(rv1, 2) * deg2rad, rawget(rv1, 3) * deg2rad)
end

--- Convert the magnitude of the vector to degrees
e2function vector toDeg(vector rv1)
	return Vector(rawget(rv1, 1) * rad2deg, rawget(rv1, 2) * rad2deg, rawget(rv1, 3) * rad2deg)
end

--------------------------------------------------------------------------------

__e2setcost(5)

--- Returns a vector in the same direction as <Input>, with a length clamped between <Min> (min) and <Max> (max)
e2function vector clamp(vector Input, Min, Max)
	if Min < 0 then Min = 0 end
	local x,y,z = rawget(Input, 1), rawget(Input, 2), rawget(Input, 3)
	local length = x*x+y*y+z*z
	if length < Min*Min then
		length = Min*(length ^ -0.5) -- Min*(length ^ -0.5) <=> Min/sqrt(length)
	elseif length > Max*Max then
		length = Max*(length ^ -0.5) -- Max*(length ^ -0.5) <=> Max/sqrt(length)
	else
		return Input
	end

	return { x*length, y*length, z*length }
end

--------------------------------------------------------------------------------

__e2setcost(1)

e2function number vector:x()
	return rawget(this, 1)
end

e2function number vector:y()
	return rawget(this, 2)
end

e2function number vector:z()
	return rawget(this, 3)
end

__e2setcost(2)

--- SET method that returns a new vector with x replaced
e2function vector vector:setX(x)
	return { x, rawget(this, 2), rawget(this, 3) }
end

--- SET method that returns a new vector with y replaced
e2function vector vector:setY(y)
	return { rawget(this, 1), y, rawget(this, 3) }
end

--- SET method that returns a new vector with z replaced
e2function vector vector:setZ(z)
	return { rawget(this, 1), rawget(this, 2), z }
end

--------------------------------------------------------------------------------

__e2setcost(6)

e2function vector round(vector rv1)
	return {
		floor(rawget(rv1, 1) + 0.5),
		floor(rawget(rv1, 2) + 0.5),
		floor(rawget(rv1, 3) + 0.5)
	}
end

e2function vector round(vector rv1, decimals)
	local shf = 10 ^ decimals
	return {
		floor(rawget(rv1, 1) * shf + 0.5) / shf,
		floor(rawget(rv1, 2) * shf + 0.5) / shf,
		floor(rawget(rv1, 3) * shf + 0.5) / shf
	}
end

e2function vector ceil( vector rv1 )
	return {
		ceil(rawget(rv1, 1)),
		ceil(rawget(rv1, 2)),
		ceil(rawget(rv1, 3))
	}
end

e2function vector ceil(vector rv1, decimals)
	local shf = 10 ^ decimals
	return {
		ceil(rawget(rv1, 1) * shf) / shf,
		ceil(rawget(rv1, 2) * shf) / shf,
		ceil(rawget(rv1, 3) * shf) / shf
	}
end

e2function vector floor(vector rv1)
	return {
		floor(rawget(rv1, 1)),
		floor(rawget(rv1, 2)),
		floor(rawget(rv1, 3))
	}
end

e2function vector floor(vector rv1, decimals)
	local shf = 10 ^ decimals
	return {
		floor(rawget(rv1, 1) * shf) / shf,
		floor(rawget(rv1, 2) * shf) / shf,
		floor(rawget(rv1, 3) * shf) / shf
	}
end

__e2setcost(10)

--- min/max based on vector length - returns shortest/longest vector
e2function vector min(vector rv1, vector rv2)
    local rv1_1 = rawget(rv1, 1)
    local rv1_2 = rawget(rv1, 2)
    local rv1_3 = rawget(rv1, 3)

    local rv2_1 = rawget(rv2, 1)
    local rv2_2 = rawget(rv2, 2)
    local rv2_3 = rawget(rv2, 3)

	local length1 = ( rv1_1 * rv1_1 + rv1_2 * rv1_2 + rv1_3 * rv1_3 ) ^ 0.5
	local length2 = ( rv2_1 * rv2_1 + rv2_2 * rv2_2 + rv2_3 * rv2_3 ) ^ 0.5
	if length1 < length2 then return rv1 else return rv2 end
end

e2function vector max(vector rv1, vector rv2)
    local rv1_1 = rawget(rv1, 1)
    local rv1_2 = rawget(rv1, 2)
    local rv1_3 = rawget(rv1, 3)

    local rv2_1 = rawget(rv2, 1)
    local rv2_2 = rawget(rv2, 2)
    local rv2_3 = rawget(rv2, 3)

	local length1 = ( rv1_1 * rv1_1 + rv1_2 * rv1_2 + rv1_3 * rv1_3 ) ^ 0.5
	local length2 = ( rv2_1 * rv2_1 + rv2_2 * rv2_2 + rv2_3 * rv2_3 ) ^ 0.5
	if length1 > length2 then return rv1 else return rv2 end
end

--- component-wise min/max
e2function vector maxVec(vector rv1, vector rv2)
    local rv1_1 = rawget(rv1, 1)
    local rv1_2 = rawget(rv1, 2)
    local rv1_3 = rawget(rv1, 3)

    local rv2_1 = rawget(rv2, 1)
    local rv2_2 = rawget(rv2, 2)
    local rv2_3 = rawget(rv2, 3)

	return {
		rv1_1 > rv2_1 and rv1_1 or rv2_1,
		rv1_2 > rv2_2 and rv1_2 or rv2_2,
		rv1_3 > rv2_3 and rv1_3 or rv2_3,
	}
end

e2function vector minVec(vector rv1, vector rv2)
    local rv1_1 = rawget(rv1, 1)
    local rv1_2 = rawget(rv1, 2)
    local rv1_3 = rawget(rv1, 3)

    local rv2_1 = rawget(rv2, 1)
    local rv2_2 = rawget(rv2, 2)
    local rv2_3 = rawget(rv2, 3)

	return {
		rv1_1 < rv2_1 and rv1_1 or rv2_1,
		rv1_2 < rv2_2 and rv1_2 or rv2_2,
		rv1_3 < rv2_3 and rv1_3 or rv2_3,
	}
end

--- Performs modulo on x,y,z separately
e2function vector mod(vector rv1, rv2)
    local rv1_1 = rawget(rv1, 1)
    local rv1_2 = rawget(rv1, 2)
    local rv1_3 = rawget(rv1, 3)

	return {
		rv1_1 >= 0 and rv1_1 % rv2 or rv1_1 % -rv2,
		rv1_2 >= 0 and rv1_2 % rv2 or rv1_2 % -rv2,
		rv1_3 >= 0 and rv1_3 % rv2 or rv1_3 % -rv2,
	}
end

--- Modulo where divisors are defined as a vector
e2function vector mod(vector rv1, vector rv2)
    local rv1_1 = rawget(rv1, 1)
    local rv1_2 = rawget(rv1, 2)
    local rv1_3 = rawget(rv1, 3)

    local rv2_1 = rawget(rv2, 1)
    local rv2_2 = rawget(rv2, 2)
    local rv2_3 = rawget(rv2, 3)

	return {
		rv1_1 >= 0 and rv1_1 % rv2_1 or rv1_1 % -rv2_1,
		rv1_2 >= 0 and rv1_2 % rv2_2 or rv1_2 % -rv2_2,
		rv1_3 >= 0 and rv1_3 % rv2_3 or rv1_3 % -rv2_3,
	}
end

--- Clamp according to limits defined by two min/max vectors
e2function vector clamp(vector value, vector min, vector max)
	local x,y,z

	local value1 = rawget(value, 1)
	local value2 = rawget(value, 2)
	local value3 = rawget(value, 3)

	local min1 = rawget(min, 1)
	local min2 = rawget(min, 2)
	local min3 = rawget(min, 3)

	local max1 = rawget(max, 1)
	local max2 = rawget(max, 2)
	local max3 = rawget(max, 3)

	if value1 < min1 then x = min1
	elseif value1 > max1 then x = max1
	else x = value1 end

	if value2 < min2 then y = min2
	elseif value2 > max2 then y = max2
	else y = value2 end

	if value3 < min3 then z = min3
	elseif value3 > max3 then z = max3
	else z = value3 end

	return {x, y, z}
end

--- Mix two vectors by a given proportion (between 0 and 1)
e2function vector mix(vector vec1, vector vec2, ratio)
	return {
		vec1[1] * ratio + vec2[1] * (1-ratio),
		vec1[2] * ratio + vec2[2] * (1-ratio),
		vec1[3] * ratio + vec2[3] * (1-ratio)
	}
end

e2function vector bezier(vector startVec, vector control, vector endVec, ratio)
	return {
		(1-ratio)^2 * startVec[1] + (2 * (1-ratio) * ratio * control[1]) + ratio^2 * endVec[1],
		(1-ratio)^2 * startVec[2] + (2 * (1-ratio) * ratio * control[2]) + ratio^2 * endVec[2],
		(1-ratio)^2 * startVec[3] + (2 * (1-ratio) * ratio * control[3]) + ratio^2 * endVec[3]
	}
end

__e2setcost(2)

--- Circular shift function: shiftR(vec(x,y,z)) = vec(z,x,y)
e2function vector shiftR(vector vec)
	return { vec[3], vec[1], vec[2] }
end

--- Circular shift function: shiftL(vec(x,y,z)) = vec(y,z,x)
e2function vector shiftL(vector vec)
	return { vec[2], vec[3], vec[1] }
end

__e2setcost(5)

--- Returns 1 if the vector lies between (or is equal to) the min/max vectors
e2function number inrange(vector vec, vector min, vector max)
    local vec1 = rawget(vec, 1)
    local vec2 = rawget(vec, 2)
    local vec3 = rawget(vec, 3)

    local min1 = rawget(min, 1)
    local min2 = rawget(min, 2)
    local min3 = rawget(min, 3)

    local max1 = rawget(max, 1)
    local max2 = rawget(max, 2)
    local max3 = rawget(max, 3)

	if vec1 < min1 then return 0 end
	if vec2 < min2 then return 0 end
	if vec3 < min3 then return 0 end

	if vec1 > max1 then return 0 end
	if vec2 > max2 then return 0 end
	if vec3 > max3 then return 0 end

	return 1
end

--------------------------------------------------------------------------------

__e2setcost(3)

e2function angle vector:toAngle()
	local angle = Vector(rawget(this, 1), rawget(this, 2), rawget(this, 3)):Angle()
	return { angle.p, angle.y, angle.r }
end

e2function angle vector:toAngle(vector up)
	local angle = Vector(rawget(this, 1), rawget(this, 2), rawget(this, 3)):AngleEx(Vector(up[1], up[2], up[3]))
	return { angle.p, angle.y, angle.r }
end

--------------------------------------------------------------------------------

local contents = {}
for k,v in pairs(_G) do
	if (k:sub(1,9) == "CONTENTS_") then
		contents[v] = k:sub(10):lower()
	end
end

local cachemeta = {}

local cache_parts_array = setmetatable({ [0] = {} }, cachemeta)
local cache_lookup_table = setmetatable({ [0] = { empty = true } }, cachemeta)
local cache_concatenated_parts = setmetatable({ [0] = "empty" }, cachemeta)

local function generateContents( n )
	local parts_array, lookup_table = {}, {}
	local ret = {}

	for i = 0,30 do
		if bit.band(n, (2^i)) ~= 0 then
			local name = contents[2^i]
			lookup_table[name] = true
			parts_array[#parts_array+1] = name
		end
	end

	concatenated_parts = table.concat(parts_array, ",")

	cache_parts_array[n] = parts_array
	cache_lookup_table[n] = lookup_table
	cache_concatenated_parts[n] = concatenated_parts
	return concatenated_parts
end

function cachemeta:__index(n)
	generateContents(n)
	return rawget(self, n)
end

__e2setcost( 20 )

e2function number pointHasContent( vector point, string has )
	local cont = cache_lookup_table[util.PointContents(Vector(point[1], point[2], point[3]))]

	has = has:gsub(" ", "_"):lower()

	for m in has:gmatch("([^,]+),?") do
		if cont[m] then return 1 end
	end

	return 0
end

__e2setcost( 15 )

e2function string pointContents( vector point )
	return cache_concatenated_parts[util.PointContents( Vector(point[1],point[2],point[3]))]
end

e2function array pointContentsArray( vector point )
	return cache_parts_array[util.PointContents( Vector(point[1],point[2],point[3]))]
end

--------------------------------------------------------------------------------

__e2setcost(15)

--- Converts a local position/angle to a world position/angle and returns the position
e2function vector toWorld( vector localpos, angle localang, vector worldpos, angle worldang )
	local localpos = Vector(rawget(localpos, 1),rawget(localpos, 2),rawget(localpos, 3)
	local localang = Angle(rawget(localang, 1),rawget(localang, 2),rawget(localang, 3)
	local worldpos = Vector(rawget(worldpos, 1),rawget(worldpos, 2),rawget(worldpos, 3)
	local worldang = Angle(rawget(worldang, 1),rawget(worldang, 2),rawget(worldang, 3)
	return LocalToWorld(localpos,localang,worldpos,worldang)
end

--- Converts a local position/angle to a world position/angle and returns the angle
e2function angle toWorldAng( vector localpos, angle localang, vector worldpos, angle worldang )
	local localpos = Vector(rawget(localpos, 1),rawget(localpos, 2),rawget(localpos, 3)
	local localang = Angle(rawget(localang, 1),rawget(localang, 2),rawget(localang, 3)
	local worldpos = Vector(rawget(worldpos, 1),rawget(worldpos, 2),rawget(worldpos, 3)
	local worldang = Angle(rawget(worldang, 1),rawget(worldang, 2),rawget(worldang, 3)
	local pos, ang = LocalToWorld(localpos,localang,worldpos,worldang)
	return {rawget(ang, "p"),rawget(ang, "y"),rawget(ang, "r")}
end

--- Converts a local position/angle to a world position/angle and returns both in an array
e2function array toWorldPosAng( vector localpos, angle localang, vector worldpos, angle worldang )
	local localpos = Vector(rawget(localpos, 1),rawget(localpos, 2),rawget(localpos, 3)
	local localang = Angle(rawget(localang, 1),rawget(localang, 2),rawget(localang, 3)
	local worldpos = Vector(rawget(worldpos, 1),rawget(worldpos, 2),rawget(worldpos, 3)
	local worldang = Angle(rawget(worldang, 1),rawget(worldang, 2),rawget(worldang, 3)
	local pos, ang = LocalToWorld(localpos,localang,worldpos,worldang)
	return {pos, {rawget(ang, "p"),rawget(ang, "y"),rawget(ang, "r")}}
end

--- Converts a world position/angle to a local position/angle and returns the position
e2function vector toLocal( vector localpos, angle localang, vector worldpos, angle worldang )
	local localpos = Vector(rawget(localpos, 1),rawget(localpos, 2),rawget(localpos, 3)
	local localang = Angle(rawget(localang, 1),rawget(localang, 2),rawget(localang, 3)
	local worldpos = Vector(rawget(worldpos, 1),rawget(worldpos, 2),rawget(worldpos, 3)
	local worldang = Angle(rawget(worldang, 1),rawget(worldang, 2),rawget(worldang, 3)
	return WorldToLocal(localpos,localang,worldpos,worldang)
end

--- Converts a world position/angle to a local position/angle and returns the angle
e2function angle toLocalAng( vector localpos, angle localang, vector worldpos, angle worldang )
	local localpos = Vector(rawget(localpos, 1),rawget(localpos, 2),rawget(localpos, 3)
	local localang = Angle(rawget(localang, 1),rawget(localang, 2),rawget(localang, 3)
	local worldpos = Vector(rawget(worldpos, 1),rawget(worldpos, 2),rawget(worldpos, 3)
	local worldang = Angle(rawget(worldang, 1),rawget(worldang, 2),rawget(worldang, 3)
	local vec, ang = WorldToLocal(localpos,localang,worldpos,worldang)
	return {rawget(ang, "p"),rawget(ang, "y"),rawget(ang, "r")}
end

--- Converts a world position/angle to a local position/angle and returns both in an array
e2function array toLocalPosAng( vector localpos, angle localang, vector worldpos, angle worldang )
	local localpos = Vector(rawget(localpos, 1),rawget(localpos, 2),rawget(localpos, 3)
	local localang = Angle(rawget(localang, 1),rawget(localang, 2),rawget(localang, 3)
	local worldpos = Vector(rawget(worldpos, 1),rawget(worldpos, 2),rawget(worldpos, 3)
	local worldang = Angle(rawget(worldang, 1),rawget(worldang, 2),rawget(worldang, 3)
	local pos, ang = WorldToLocal(localpos,localang,worldpos,worldang)
	return {pos, {rawget(ang, "p"),rawget(ang, "y"),rawget(ang, "r")}}
end

--------------------------------------------------------------------------------
-- Credits to Wizard of Ass for bearing(v,a,v) and elevation(v,a,v)

e2function number bearing(vector originpos,angle originangle, vector pos)
	pos = WorldToLocal(Vector(rawget(pos, 1),rawget(pos, 2),rawget(pos, 3),Angle(0,0,0),Vector(rawget(originpos, 1),rawget(originpos, 2),rawget(originpos, 3),Angle(rawget(originangle, 1),rawget(originangle, 2),rawget(originangle, 3))
	return rad2deg*-atan2(rawget(pos, "y"), rawget(pos, "x")
end

e2function number elevation(vector originpos,angle originangle, vector pos)
	pos = WorldToLocal(Vector(rawget(pos, 1),rawget(pos, 2),rawget(pos, 3),Angle(0,0,0),Vector(rawget(originpos, 1),rawget(originpos, 2),rawget(originpos, 3),Angle(rawget(originangle, 1),rawget(originangle, 2),rawget(originangle, 3))
	local len = pos:Length()
	if (len < delta) then return 0 end
	return rad2deg*asin(rawget(pos, "z") / len)
end

e2function angle heading(vector originpos,angle originangle, vector pos)
	pos = WorldToLocal(Vector(rawget(pos, 1),rawget(pos, 2),rawget(pos, 3),Angle(0,0,0),Vector(rawget(originpos, 1),rawget(originpos, 2),rawget(originpos, 3),Angle(rawget(originangle, 1),rawget(originangle, 2),rawget(originangle, 3))

	local bearing = rad2deg*-atan2(rawget(pos, "y"), rawget(pos, "x"))

	local len = pos:Length()
	if (len < delta) then return { 0, bearing, 0 } end
	return { rad2deg*asin(rawget(pos, "z") / len), bearing, 0 }
end

--------------------------------------------------------------------------------

__e2setcost( 10 )


e2function number vector:isInWorld()
	if util.IsInWorld(Vector(rawget(this, 1), rawget(this, 2), rawget(this, 3)) then return 1 else return 0 end
end

__e2setcost( 5 )

--- Gets the vector nicely formatted as a string "[X,Y,Z]"
e2function string toString(vector v)
	return ("[%s,%s,%s]"):format(rawget(v, 1),rawget(v, 2),rawget(v, 3)
end

--- Gets the vector nicely formatted as a string "[X,Y,Z]"
e2function string vector:toString() = e2function string toString(vector v)
