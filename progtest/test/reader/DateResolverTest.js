/**
 * Tests DateResolver
 *
 *  Copyright (C) 2014-2022 Ryan Specialty Group, LLC.
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
const { Class }  = require( 'easejs' );
const TestCase   = require( '../../src/TestCase' );
const TestReader = require( '../../src/reader/TestReader' );
const Sut        = require( '../../src/reader/DateResolver' );

const MockTestReader = Class.implement( TestReader ).extend(
{
    constructor( parsed_data, expected_load )
    {
        this.parsedData   = parsed_data;
        this.expectedLoad = expected_load;
    },

    'virtual public loadCases'( given )
    {
        expect( given ).to.equal( this.expectedLoad );
        return this.parsedData;
    }
} );


describe( "DateResolver", () =>
{
    [ 'data', 'expect' ].forEach( field =>
    {
        it( `converts ${field} dates into Unix timestamps`, () =>
        {
            const date = '10/25/1989';
            const time = ( new Date( date ) ).getTime() / 1000;

            // tests scalar, vector, matrix; mixed with non-constants
            const parsed_data = [
                TestCase(
                    { [field]: {
                        foo: [ 5, 'NOTADATE', date ],
                    } }
                ),
            ];

            const expected = [
                TestCase(
                    { [field]: {
                        foo: [ 5, 'NOTADATE', time ],
                    } }
                ),
            ];

            // anything just to proxy
            const given_yaml = 'fooml';

            const result = MockTestReader
                .use( Sut )( parsed_data, given_yaml )
                .loadCases( given_yaml );

            result.forEach(
                ( tcase, i ) => expect( tcase[ field ] )
                    .to.deep.equal( expected[ i ][ field ] )
            );
        } );
    } );
} );
