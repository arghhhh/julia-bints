

module Bints

struct Bint{lo,hi} <: Integer
        n

        # make a Bint with a given range and value
        function Bint{lo,hi}( n::Integer ) where {lo,hi }
                if !( lo <= n <= hi )
                        throw( OverflowError( "$n out of range $(lo)..$(hi) ") )
                end
                new{lo,hi}( n )
        end

        # make a Bint from one with a narrower range
        function Bint{lo,hi}( n::Bint{n_lo,n_hi} ) where {lo,hi, n_lo, n_hi }
                if !( lo <= n_lo  && hi >= n_hi )
                        throw( OverflowError( "$n out of range $(lo)..$(hi) ") )
                end
                new{lo,hi}( n.n )
        end     



end

# these return types not values:

# Bint type corresponding to nbits unsigned
function uBint( nbits::Int )
        lo = 0
        hi = (1<<nbits)-1
        return Bint{lo,hi}
end

# return the Bint type corresponding to the given range (as a type) as unsigned
function uBint( ::Type{Bint{lo,hi} } ) where { lo, hi }
        @assert lo >= 0
        lo1 = 0
        hi1 = round( Int, exp2( ceil( log2( hi + 1.0 ) ) ) ) - 1
        return Bint{lo1,hi1}
end

function sBint( nbits::Int )
        lo = -( 1<<(nbits-1) )
        hi =  ( 1<<(nbits-1) ) - 1
        return Bint{lo,hi}
end

function sBint( ::Type{Bint{lo,hi} } ) where { lo, hi }
        h = max( -lo - 1 , hi )
        n = round( Int, exp2( ceil( log2( h + 1.0 ) ) ) )
        lo1 = -n
        hi1 = n-1
        return Bint{lo1,hi1}
end




end # module

