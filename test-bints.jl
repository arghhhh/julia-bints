
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

        @test zero( Bint{-5,5} ) == Bint{-5,5}(0)
        @test one( Bint{-5,5} ) == Bint{-5,5}(1)
        @test_throws OverflowError zero( Bint{2,5} )

        @test clamp( 10,  Bint{-5,5} ) == Bint{-5,5}(5)
        @test Int( Bint{-10,10}(5) ) == 5
        @test Integer( Bint{-10,10}(5) ) == 5

        @test Bint{-10,10}( 10, Val{false}() ) == Bint{-10,10}( 10 )
        @test Float64( Bint{-10,10}( 6 ) ) == 6.0

        @test Bint{-10,10}(1) + Bint{-10,10}(1) == Bint{-20,20}(2)
        @test Bint{-10,10}(2) - Bint{-10,10}(1) == Bint{-20,20}(1)
        @test Bint{-10,10}(2) * Bint{-10,10}(2) == Bint{-100,100}(4)

        @test sprint( show, uBint(4)    ) == "uBint(4)"
        @test sprint( show, sBint(4)    ) == "sBint(4)"
        @test sprint( show, uBint(4)(2) ) == "uBint(4)(2)"
        @test sprint( show, sBint(4)(3) ) == "sBint(4)(3)"
        @test sprint( show, Bint{-10,10}(2) ) == "Bint{-10,10}(2)"
        @test sprint( show, Bint{-3,3}(2) ) == "Bint{-3,3}(2)"
end






nothing

