/**
 * Tests TestReader
 *
 *  Copyright (C) 2014-2021 Ryan Specialty Group, LLC.
 *
 *  This file is part of TAME.
 *
 *  TAME is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

"use strict";

const { expect } = require( 'chai' );
const Sut = require( '../../src/reader/YamlTestReader' );


describe( "YamlTestReader", () =>
{
    it( "parses given yaml", () =>
    {
        const yaml = "foo: bar";

        const parsed = [
            {
                description: "first desc",
                data:        { "foo": "bar" },
                expect:      { "bar": "baz" },
            },
        ];

        const case_ctor = ( data ) => ( { ok: data } );

        const mock_parser = {
            safeLoad( given )
            {
                expect( given ).to.equal( yaml );
                return parsed;
            }
        };

        expect( Sut( mock_parser, case_ctor ).loadCases( yaml ) )
            .to.deep.equal( [ { ok: parsed[0] } ] );
    } );


    it( "produces empty array given no tests", () =>
    {
        const mock_parser = { safeLoad: () => null };
        const case_ctor   = () => null;   // unused

        expect( Sut( mock_parser, case_ctor ).loadCases( "" ) )
            .to.deep.equal( [] );
    } );
} );
