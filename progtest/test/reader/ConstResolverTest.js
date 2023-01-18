/**
 * Tests ConstResolver
 *
 *  Copyright (C) 2014-2023 Ryan Specialty, LLC.
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
const Sut        = require( '../../src/reader/ConstResolver' );

const StubTestReader = Class.implement( TestReader ).extend(
{
    constructor( parsed_data )
    {
        this.parsedData = parsed_data;
    },

    'virtual public loadCases'( _ )
    {
        return this.parsedData;
    }
} );


describe( "ConstResolver", () =>
{
    [ 'data', 'expect' ].forEach( field =>
    {
        it( `replaces known ${field} constants from program`, () =>
        {
            const program = {
                rater: {
                    consts: { FOO: 1, BAR: 2 },
                },
            };

            // tests scalar, vector, matrix; mixed with non-constants
            const parsed_data = [
                TestCase( { [field]: { foo: 'FOO', bar: 4 } } ),
                TestCase(
                    { [field]: {
                        foo: [ 'FOO', 'BAR', 5 ],
                        bar: [ [ 'FOO', 3 ], [ 'FOO', 'BAR' ] ],
                    } }
                ),
            ];

            const { FOO, BAR } = program.rater.consts;

            const expected = [
                TestCase( { [field]: { foo: FOO, bar: 4 } } ),
                TestCase(
                    { [field]: {
                        foo: [ FOO, BAR, 5 ],
                        bar: [ [ FOO, 3 ], [ FOO, BAR ] ],
                    } }
                ),
            ];

            // anything just to proxy
            const given_yaml = 'fooml';

            const result = StubTestReader
                .use( Sut( program ) )( parsed_data )
                .loadCases( given_yaml );

            result.forEach(
                ( tcase, i ) => expect( tcase[ field ] )
                    .to.deep.equal( expected[ i ][ field ] )
            );
        } );


        it( `throws error on unknown $field constant`, () =>
        {
            const program     = { rater: { consts: {} } };
            const parsed_data = [ TestCase( { [field]: { foo: 'UNKNOWN' } } ) ];

            expect(
                () => StubTestReader.use( Sut( program ) )( parsed_data )
                    .loadCases( '' )
            ).to.throw( Error, 'UNKNOWN' );
        } );
    } );
} );
