/**
 * Test case runner script
 *
 *  Copyright (C) 2018 R-T Specialty, LLC.
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

const program  = require( process.argv[ 2 ] );
const filename = process.argv[ 3 ];

const fs          = require( 'fs' );
const yaml_reader = require( 'js-yaml' );

const {
    TestCase,
    TestRunner,

    reader: {
        ConstResolver,
        DateResolver,
        YamlTestReader
    },

    reporter: {
        ConsoleTestReporter
    },
} = require( '../src' );

const runner = TestRunner(
    ConsoleTestReporter( process.stdout ),
    program
);

const reader = YamlTestReader
    .use( DateResolver )
    .use( ConstResolver( program ) )
    ( yaml_reader, TestCase );

const cases = reader.loadCases(
    fs.readFileSync( filename, 'utf8' )
);

const results = runner.runTests( cases );
