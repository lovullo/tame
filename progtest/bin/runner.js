/**
 * Test case runner script
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

const program  = require( process.cwd() + '/' + process.argv[ 2 ] );
const filename = process.argv[ 3 ];

const fs = require( 'fs' );

const case_yaml = fs.readFileSync( filename, 'utf8' );

const runner = require( '../src/env' ).console(
    program, process.stdout
);

runner( case_yaml )
    .then( results =>
    {
        const failed = results.some(
            ( { failures } ) => failures.length
        );

        process.exit( +failed );
    } )
    .catch( e =>
    {
        console.error( e );
        process.exit( 1 );
    } );

