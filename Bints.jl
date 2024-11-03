

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

        function Bint{lo,hi}( n::Integer, checked::Val{false} ) where {lo,hi }
                new{lo,hi}( n )
        end

end

# add functionality expected of integers:

import Base.zero, Base.one
# these act on types not values:
Base.zero( ::Type{Bint{lo,hi}} ) where {lo,hi} = Bint{lo,hi}(0)
Base.one(  ::Type{Bint{lo,hi}} ) where {lo,hi} = Bint{lo,hi}(1)

import Base.typemin, Base.typemax
Base.typemin( ::Type{Bint{lo,hi}} ) where {lo,hi} = Bint{lo,hi}(lo)
Base.typemax( ::Type{Bint{lo,hi}} ) where {lo,hi} = Bint{lo,hi}(hi)

# the next few lines allows comparisons with integers and allows clamp to work
import Base.promote_rule
promote_rule( ::Type{Bint{lo,hi}} , ::Type{S} ) where {lo,hi,S} = begin
        T = promote_type( typeof(lo), typeof(hi) )
        promote_type(T,S)
end


# extract the integer without the bounds:
(::Type{I})(n::Bint{lo,hi}) where {lo,hi,I<:Integer} = I(n.n)
Integer( n::Bint{lo,hi} ) where {lo,hi} = n.n



Bint( n::Integer ) = Bint{n,n}( n, Val{false}() )

Base.Float64( n::Bint ) = Float64( n.n )

# arithmetic:

import Base.:+
function Base.:+( n1::Bint{lo1,hi1}, n2::Bint{lo2,hi2} ) where {lo1,hi1,lo2,hi2}
 
        lo = Base.Checked.checked_add( lo1, lo2)
        hi = Base.Checked.checked_add( hi1, hi2)

        # the expectation is that the above is compiled to constants
        # and this entire function reduces to a single add without range checks
        return Bint{ lo, hi}( n1.n + n2.n, Val{false}() )
end

import Base.:-
function Base.:-( n1::Bint{lo1,hi1}, n2::Bint{lo2,hi2} ) where {lo1,hi1,lo2,hi2}
 
        lo = Base.Checked.checked_sub( lo1, hi2)
        hi = Base.Checked.checked_sub( hi1, lo2)

        return Bint{ lo, hi }( n1.n - n2.n, Val{false}() )
end

import Base.:*
function Base.:*( n1::Bint{lo1,hi1}, n2::Bint{lo2,hi2} ) where {lo1,hi1,lo2,hi2}
 
        y1 = Base.Checked.checked_mul( lo1, lo2)
        y2 = Base.Checked.checked_mul( lo1, hi2)
        y3 = Base.Checked.checked_mul( hi1, lo2)
        y4 = Base.Checked.checked_mul( hi1, hi2)

        lo = min( y1, y2, y3, y4 )
        hi = max( y1, y2, y3, y4 )

        return Bint{ lo, hi }( n1.n * n2.n, Val{false}() )
end



# unary negate
import Base.:-
function Base.:-( n1::Bint{lo1,hi1} ) where {lo1,hi1}
        return Bint{ -hi1, -lo1 }( -n1.n )
end

# ones complement:
import Base.:~
function Base.:~( n1::Bint{lo1,hi1} ) where {lo1,hi1}
        return Bint{ ~hi1, ~lo1 }( ~n1.n )
end




# can have left shift because it should be lossless
# but don't have right shift because it would be lossy
# this function is used when adding fixpts with different exponents
import Base.:<<
function Base.:<<( n1::Bint{lo,hi}, n2::Int64 ) where {lo,hi}

        # assuming that none of the following lines overflow

        n = n1.n << n2

        lo1 = lo << n2
        hi1 = hi << n2
        return Bint{lo1,hi1}( n )
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

