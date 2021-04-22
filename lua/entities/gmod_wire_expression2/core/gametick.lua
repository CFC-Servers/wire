/******************************************************************************\
  Game tick callback support
\******************************************************************************/
rawget = rawget
rawset = rawset
pairs = pairs

local registered_chips = {}
local tickrun = 0

registerCallback("destruct",function(self)
	rawset( registered_chips, self.entity, nil )
end)

__e2setcost(1)

--- If <activate> != 0 the expression will execute once every game tick
e2function void runOnTick(activate)
    if activate ~= 0 then
        rawset( registered_chips, self.entity, true )
    else
		rawset( registered_chips, self.entity, nil )
    end
end

--- Returns 1 if the current execution was caused by "runOnTick"
e2function number tickClk()
	return rawget( self.data, "tickrun" ) and 1 or 0
end

local function Expression2TickClock()
	local ents = {}

	-- this additional step is needed because we cant modify registered_chips while it is being iterated.
	local i = 1
	for entity in pairs(registered_chips) do
		if entity:IsValid() then
			rawset( ents, i, entity )
			i = i + 1
		end
	end

	for n = 1, i do
	    local entity = rawget( ents, n )
	    local data = rawget( entity.context, "data" )

		rawset( data, "tickrun", true )
		entity:Execute()
		rawset( data, "tickrun", nil )
	end
end
hook.Add("Think", "Expression2TickClock", Expression2TickClock)
timer.Create("Expression2TickClock", 5, 0, function()
	hook.Add("Think", "Expression2TickClock", Expression2TickClock)
end)
