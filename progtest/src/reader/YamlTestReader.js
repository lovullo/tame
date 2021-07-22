/**
 * YAML test case reader
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

const { Class }  = require( 'easejs' );
const TestReader = require( './TestReader' );


module.exports = Class( 'YamlTestReader' )
    .implement( TestReader )
    .extend(
{
    /**
     * YAML parser
     *
     * @type {Object}
     */
    'private _yamlParser': null,

    /**
     * TestCase constructor
     *
     * @type {function(Object)}
     */
    'private _createTestCase': null,


    /**
     * Initialize with YAML parser
     *
     * The parser must conform to the API of `js-yaml`.
     *
     * @param {Object}           yaml_parser    YAML parser
     * @param {function(Object)} test_case_ctor TestCase constructor
     */
    constructor( yaml_parser, test_case_ctor )
    {
        this._yamlParser     = yaml_parser;
        this._createTestCase = test_case_ctor;
    },


    /**
     * Load test cases from a YAML string
     *
     * The produced object will be an array of cases, each containing a
     * `description`, `data`, and `expect`.
     *
     * @param {string} src source YAML
     *
     * @return {Array<TestCase>} array of test cases
     */
    'virtual public loadCases'( yaml )
    {
        const data = ( this._yamlParser.safeLoad( yaml ) || [] )
              .map( this._createTestCase );

        return data;
    },
} );
