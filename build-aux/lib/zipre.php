<?php
/**
 * Generate regular expressions to match a list of zip codes
 *
 *   Copyright (C) 2014-2022 Ryan Specialty Group, LLC.
 *
 *   This program is free software: you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation, either version 3 of the License, or
 *   (at your option) any later version.
 *
 *   This program is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.
 *
 *   You should have received a copy of the GNU General Public License
 *   along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */


function gen_re_quick( $data )
{
    $re = ( '^' . gen_re( $data, 0 ) );

    // attempt to simplify the regex (we're not going to put a lot of effort into
    // this)
    return re_simplify( $re );
}


function gen_re( $data, $offset )
{
    // if we've reached the end of the zip length, or if there's no more zips to
    // look at, then stop
    if ( ( count( $data ) === 0 )
        || ( $offset === 5 )
    )
    {
        return '';
    }

    $out = '(';

    // loop through each digit at the current offset
    $last = '';
    foreach ( $data as $zip )
    {
        if ( !( isset( $zip[ $offset ] ) ) )
        {
            continue;
        }

        $digit = $zip[ $offset ];

        // if we've already seen this digit in the current position, then
        // continue
        if ( $digit === $last )
        {
            continue;
        }

        // we're going to recurse now, delimiting allowable digits with pipes
        // (for 'OR'); we'll recurse on a sublist that matches the zip up to
        // (and including) the current digit (to do this, note that we only need
        // to check the current digit, since our current list is already a
        // sublist of the parent list up to the current point)
        $prefix = substr( $zip, 0, $offset + 1 );

        $out .= ( $last === '' ) ? '' : '|';
        $out .= $digit . gen_re(
            filter_data( $data, $digit, $offset ),
            ( $offset + 1 )
        );

        $last = $digit;
    }

    return $out . ')';
}

function filter_data( $data, $chr, $offset )
{
    $ret = array();

    foreach ( $data as $val )
    {
        if ( $val[ $offset] === $chr )
        {
            $ret[] = $val;
        }
    }

    return $ret;
}

function re_simplify( $re )
{
    // the only simplification we currently do is joining sequential digit ORs
    // into a character range (e.g. (1|2|3|4) becomes [1-4])
    return preg_replace_callback( '/\([0-9](\|[0-9])*\)/', function( $results )
    {
        $match  = $results[ 0 ];
        $digits = explode( '|', str_replace( array( '(', ')' ), '', $match ) );

        // are the digits sequential (we will only perform this optimization if
        // there's more than 3 digits, since otherwise the replacement would
        // result in a string of equal or greater length)?
        if ( ( count( $digits ) > 3 ) && is_seq( $digits ) )
        {
            return sprintf( '[%d-%d]',
                $digits[ 0 ],
                $digits[ count( $digits ) - 1 ]
            );
        }
        elseif ( count( $digits ) === 1 )
        {
            // if there's only one digit, then that's all we need to return
            return $digits[ 0 ];
        }

        return '[' . implode( '', $digits ) . ']';
    }, $re );
}

function is_seq( $digits, $last = '' )
{
    // stop recursing once we're out of digits
    if ( count( $digits ) === 0 )
    {
        return true;
    }

    // grab the current digit and remove it from the list (this has the effect
    // of both cons and cdr)
    $digit = (int)( array_shift( $digits ) );

    // consider this a sequence if this digit is one more than the last (or if
    // there is no last digit) and if the following digit is sequential
    return ( ( $last === '' ) || ( $digit === ( $last + 1) ) )
        && is_seq( $digits, $digit );
}
