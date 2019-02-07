<?xml version="1.0" encoding="ISO-8859-1"?>
<!--
  Compile standalone JavaScript program suitable for execution

  Copyright (C) 2014-2019 Ryan Specialty Group, LLC.

    This file is part of TAME.

    TAME is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see
    <http://www.gnu.org/licenses/>.
-->

<stylesheet version="2.0"
            xmlns="http://www.w3.org/1999/XSL/Transform"
            xmlns:lv="http://www.lovullo.com/rater"
            xmlns:lvp="http://www.lovullo.com"
            xmlns:lvm="http://www.lovullo.com/rater/map"
            xmlns:lvmc="http://www.lovullo.com/rater/map/compiler"
            xmlns:c="http://www.lovullo.com/calc"
            xmlns:l="http://www.lovullo.com/rater/linker"
            xmlns:compiler="http://www.lovullo.com/rater/compiler"
            xmlns:calc-compiler="http://www.lovullo.com/calc/compiler"
            xmlns:util="http://www.lovullo.com/util"
            xmlns:ext="http://www.lovullo.com/ext"
            xmlns:preproc="http://www.lovullo.com/rater/preproc">


<output
  indent="yes"
  omit-xml-declaration="yes"
  />

<include href="include/dslc-base.xsl" />

<!-- compiler -> JS -->
<include href="compiler/linker.xsl" />
<include href="compiler/map.xsl" />
<include href="include/depgen.xsl" />

<!-- path to program XML -->
<param name="path-program-ui" />

<template match="/" priority="5">
  <!-- the rater itself -->
  <text>var rater = </text>
    <value-of disable-output-escaping="yes" select="/lv:package/l:exec/text()" />
  <text>; </text>

  <!-- maps may or may not exist -->
  <variable name="map" select="/lv:package/l:map-exec" />
  <variable name="retmap" select="/lv:package/l:retmap-exec" />

  <!-- store a reference to the mapper in rater.fromMap() -->
  <text>rater.fromMap = </text>
    <choose>
      <when test="$map">
        <value-of disable-output-escaping="yes" select="$map/text()" />
      </when>

      <!-- no map available -->
      <otherwise>
        <!-- simply invoke the conintuation with the provided data -->
        <text>function(d,c){c(d);}</text>
      </otherwise>
    </choose>
  <text>; </text>

  <!-- return map -->
  <text>rater._retmap = </text>
    <choose>
      <when test="$retmap">
        <value-of disable-output-escaping="yes" select="$retmap/text()" />
      </when>

      <!-- no map available -->
      <otherwise>
        <!-- simply invoke the conintuation with the provided data -->
        <text>function(d,c){c(d);}</text>
      </otherwise>
    </choose>
  <text>; </text>

  <!-- we'll export a version that automatically performs the mapping -->
  <text>module.exports = function( args_base ) { </text>
    <text>var ret; rater.fromMap( args_base, function( args ) {</text>
    <text>
      var rater_result = rater( args );

      // perf counter
      var start = ( new Date() ).getTime();

      rater._retmap( rater_result.vars, function( result )
      {
        // add the final premium
        result.premium   = rater_result.premium;
        result.__classes = rater_result.classes;

        // process the rating worksheet
        try
        {
          result.__worksheet = process_worksheet(
            rater.worksheet,
            rater_result.vars,
            rater.consts,
            rater_result.debug,
            rater_result.premium
          );
        }
        catch ( e )
        {
          result.__worksheet = [ 'Failed: ' + e.message ];
        }
        ret = result;
      } );

      // add performance data
      var end  = ( new Date() ).getTime(),
          time = ( ( new Date() ).getTime() - start );

      ret.__perf = {
        time: {
          start: start,
          end:   end,
          total: time
        }
      };
    </text>
    <text>} );</text>

    <text>return ret;</text>
  <text>}; </text>

  <text>
    function process_worksheet( worksheet, vars, consts, debug, premium )
    {
      var ret = {};

      for ( var name in worksheet )
      {
        var data   = Array.prototype.slice.call( worksheet[ name ] ),
            disp   = data[0],
            calc   = data[1],
            always = data[2];

        ret[ name ] = [
          disp,
          process_wdisplay_set( [calc], vars, consts, debug ),

          ( ( name === 'yield' )
            ? premium
            : ( vars[ name ] || consts[ name ] )
          ),

          ( always === 'true' )
        ];
      }

      return ret;
    }

    function process_wdisplay( data, vars, consts, debug )
    {
      if ( data === null )
      {
        return null;
      }

      var name = data[ 0 ],
          desc = data[ 1 ],
          sub  = data[ 2 ],
          val  = data[ 3 ]; // may not exist

      return [
        name,
        desc,
        process_wdisplay_set( sub, vars, consts, debug ),
        val || process_wval( name, desc, vars, consts, debug )
      ];
    }


    function process_wval( type, desc, vars, consts, debug )
    {
      if ( desc.runtime )
      {
          type = 'runtime';
      }

      switch ( type )
      {
        case 'apply':
        case 'cases':
        case 'case':
        case 'otherwise':
        case 'runtime':
          return ( debug[ desc._id ] );

        case 'value-of':
          return ( vars[ desc.name ] || consts[ desc.name ] );

        default:
          return '';
      }
    }


    function process_wdisplay_set( sub, vars, consts, debug )
    {
      var ret = [],
          i   = sub.length;

      while ( i-- )
      {
        if ( sub[ i ] === undefined )
        {
          continue;
        }

        ret[ i ] = process_wdisplay( sub[ i ], vars, consts, debug );
      }

      return ret;
    }
  </text>

  <!-- expose the raw, unmapped rater -->
  <text>module.exports.rater = rater;</text>
</template>

</stylesheet>
