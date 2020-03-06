/**
 * Tests TestCase
 *
 *  Copyright (C) 2014-2020 Ryan Specialty Group, LLC.
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
const Sut = require( '../src/TestCase' );


describe( "TestCase", () =>
{
    it( "allows retrieving raw data", () =>
    {
        const data = {
            description: "Foo bar",
            data: { foo: [ 5 ] },
            expect: { bar: [ 1 ] },
        };

        const sut = Sut( data );

        expect( sut.description ).to.equal( data.description );
        expect( sut.data ).to.deep.equal( data.data );
        expect( sut.expect ).to.deep.equal( data.expect );
    } );


    it( "provides sane defaults for missing data", () =>
    {
        const sut = Sut( {} );

        expect( sut.description ).to.equal( "" );
        expect( sut.data ).to.deep.equal( {} );
        expect( sut.expect ).to.deep.equal( {} );
    } );


    describe( "#mapEachValue", () =>
    {
        it( "visits each 'data' and 'expect' value", () =>
        {
            // tests scalar, vector, matrix; mixed with non-constants
            const testcase = {
                description: 'test desc',

                data: {
                    foo: 'bar',
                    bar: [ 'baz', 'quux' ],
                    baz: [ [ 'quuux', 'foox' ], [ 'moo', 'cow' ] ],
                },
                expect: {
                    quux:  'out',
                    quuux: [ 'of', 'names' ],
                },
            };

            const expected = {
                data: {
                    foo: 'OKbar',
                    bar: [ 'OKbaz', 'OKquux' ],
                    baz: [ [ 'OKquuux', 'OKfoox' ], [ 'OKmoo', 'OKcow' ] ],
                },
                expect: {
                    quux:  'OKout',
                    quuux: [ 'OKof', 'OKnames' ],
                },
            };

            const result = Sut( testcase ).mapEachValue( val => `OK${val}` );

            // derived from the original
            expect( result.description ).to.equal( testcase.description );
            expect( result.data ).to.deep.equal( expected.data );
            expect( result.expect ).to.deep.equal( expected.expect );

            // but not the original (should return a new object)
            expect( result.data ).to.not.equal( testcase.data );
            expect( result.expect ).to.not.equal( testcase.expect );
        } );
    } );
} );
