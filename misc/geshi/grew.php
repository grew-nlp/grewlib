<?php
/*************************************************************************************
 * grew.php
 * -------
 * Author: Bruno GUILLAUME (Bruno.Guillaume@loria.fr)
 * Release Version: 1.0.0.0
 * Date Started: 2010/12/14
 *
 * Grew language file for GeSHi.
 *
 ************************************************************************************/

$language_data = array (
    'LANG_NAME' => 'grew',
    'COMMENT_SINGLE' => array('%'),
    'COMMENT_MULTI' => array('/*' => '*/', '%--' => '--%'),
    'COMMENT_REGEXP' => array(),
    'CASE_KEYWORDS' => GESHI_CAPS_NO_CHANGE,
    'QUOTEMARKS' => array('"', "'"),
    'ESCAPE_CHAR' => '',
    'KEYWORDS' => array(
        1 => array(
            'graph', 'match', 'without', 'commands'
            ),
        2 => array('del_edge', 'add_edge', 'del_node', 'add_node', 'del_feat', 'add', 'shift', 'shift_in', 'shift_out', 'activate'
            ),
        3 => array('rule', 'lex_rule', 'module', 'labels', 'sequences', 'features')
        ),
    'SYMBOLS' => array(
        '{', '}',';','-','=','<','>',','
        ),
    'CASE_SENSITIVE' => array(
        GESHI_COMMENTS => false,
        1 => true,
        2 => true
        ),
    'STYLES' => array(
        'KEYWORDS' => array(
            1 => 'color: #00FF00;',
            2 => 'color: #FF0000;',
            3 => 'color: yellow;'
            ),
        'COMMENTS' => array(),
        'ESCAPE_CHAR' => array(),
        'BRACKETS' => array(
            0 => 'color: #00AA00;'
            ),
        'STRINGS' => array(
            0 => 'color: #ff0000;'
            ),
        'NUMBERS' => array(
            0 => 'color: #cc66cc;'
            ),
        'METHODS' => array(
            ),
        'SYMBOLS' => array(
            0 => 'color: #00AA00;'
            ),
        'SCRIPT' => array(
            ),
        'REGEXPS' => array(
            0 => 'color: #cc00cc;'
            )
        ),
    'URLS' => array(
        1 => '',
        2 => ''
        ),
    'OOLANG' => false,
    'OBJECT_SPLITTERS' => array(
        ),
    'REGEXPS' => array(
        0 => '\#[0-9][0-9][0-9][0-9][0-9][0-9]'
        ),
    'STRICT_MODE_APPLIES' => GESHI_NEVER,
    'SCRIPT_DELIMITERS' => array(
        ),
    'HIGHLIGHT_STRICT_BLOCK' => array(
        ),
    'TAB_WIDTH' => 4,
    'PARSER_CONTROL' => array(
        'KEYWORDS' => array(
            'DISALLOWED_AFTER' => '(?![a-zA-Z0-9_\|%\\-&\.])'
        )
    )
);

?>
