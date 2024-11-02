
function add_to_Julia_path( p )
        if p ∉ LOAD_PATH
                push!( LOAD_PATH, p )
        end
end
add_to_Julia_path( "." )

import Bints 
import Bints.Bint 
import Bints.uBint 
import Bints.sBint

using Test

@testset "Bints" begin

        @test Bint{-10,10}(5).n == 5
        @test_throws OverflowError Bint{-10, 10}( 20 )
        @test Bint{-100,100}( Bint{-10,10}(5) ) == Bint{-100,100}(5)

        @test uBint(4) == Bint{0,15}
        @test uBint( Bint{2,10} ) == Bint{0,15}
        @test sBint(4) == Bint{-8,7}
        @test sBint( Bint{2,5} ) == Bint{-8,7}

end

nothing
