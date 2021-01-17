function sepList(rule, separator) {
    return seq(rule, repeat(seq(separator, rule)));
}

function sepList1(rule, separator) {
    return seq(rule, repeat1(seq(separator, rule)));
}

// function comm($) {
//     return optional($.COMMENT);
// }

function optInd($) {
    // return seq($.COMMENT, optional($.IND));
    return optional($.IND);
}

function optPar($) {
    return optional(choice($.IND_GE, $.IND_EQ));
}

function paramListColon($) {
    // paramListColon = paramList? (':' optInd typeDesc)?
    return seq(
        optional($.paramList),
        optional(seq(':', optInd($), $.typeDesc))
    );
}
// paramListArrow = paramList? ('->' optInd typeDesc)?
function paramListArrow($) {
    return seq(
      optional($.paramList),
      optional(seq('->', optInd($), $.typeDesc))
    );
}

// indAndComment = (IND{>} COMMENT)? | COMMENT?
function indAndComment($) {
    return choice(optional(seq($.IND_GE, $.COMMENT)), $.COMMENT);
}

function section($, rule) {
    return choice(
        rule, seq($.IND_GE, sepList1(rule, $.IND_EQ), $.DED)
    );
}

module.exports = grammar({
    name: 'nim',
    externals: $ => [
        $.IND_GE,
        $.IND_EQ,
        $.DED,
        $.COMMENT,
        $.CHAR_LIT,
        $.STR_LIT,
        $.TRIPLESTR_LIT,
        $.RSTR_LIT,
        $.GENERALIZED_STR_LIT,
        $.GENERALIZED_TRIPLESTR_LIT
    ],

    extras: $ => [
        $.COMMENT,
        /[\s\f]/
    ],

    word: $ => $.IDENT,

    rules: {
        // WARNING
        // module = stmt ^* (';' / IND{=})
        module: $ => repeat($.stmt),

        // comma = ',' COMMENT?
        // comma: $ => ',',

        // semicolon = ';' COMMENT?
        // semicolon: $ => ';',

        // colon = ':' COMMENT?
        // colon: $ => ':',

        // colcom = ':' COMMENT?
        // colcom: $ => ':',

        KEYW: $ => prec(2, choice(
            'addr', 'and', 'as', 'asm', 'bind', 'block', 'break', 'case',
            'cast', 'concept', 'const', 'continue', 'converter', 'defer',
            'discard', 'distinct', 'div', 'do', 'elif', 'else', 'end',
            'enum', 'except', 'export', 'finally', 'for', 'from', 'func',
            'if', 'import', 'in', 'include', 'interface', 'is', 'isnot',
            'iterator', 'let', 'macro', 'method', 'mixin', 'mod', 'nil',
            'not', 'notin', 'object', 'of', 'or', 'out', 'proc', 'ptr',
            'raise', 'ref', 'return', 'shl', 'shr', 'static', 'template',
            'try', 'tuple', 'type', 'using', 'var', 'when', 'while', 'xor',
            'yield',
        )),

        IDENT: $ => /[A-Za-z\x80-\xFF]([_A-Za-z0-9\x80-\xFF])*/,

        // operator =  OP0 | OP1 | OP2 | OP3 | OP4 | OP5 | OP6 | OP7 | OP8 | OP9
        //          | 'or' | 'xor' | 'and'
        //          | 'is' | 'isnot' | 'in' | 'notin' | 'of' | 'as' | 'from'
        //          | 'div' | 'mod' | 'shl' | 'shr' | 'not' | 'static' | '..'
        operator: $ => choice(
            $.OP0,
            $.OP1,
            $.OP2,
            $.OP3,
            $.OP4,
            $.OP5,
            $.OP6,
            $.OP7,
            $.OP8,
            $.OP9
            // 'or',
            // 'xor',
            // 'and',
            // 'is',
            // 'isnot',
            // 'in',
            // 'notin',
            // 'of',
            // 'as',
            // 'from',
            // 'div',
            // 'mod',
            // 'shl',
            // 'shr',
            // 'not',
            // 'static',
            // '..'
        ),

        OPR: $ => /[=+\-*\/<>@$~&%|!?^.:\\]/,
        OP10: $ => /[$^][=+\-*\/<>@$~&%|!?^.:\\]*/,
        OP9: $ => choice(
            'div',
            'mod',
            'shl',
            'shr',
            /([*\/%][+\-*\/<>@$~&%|!?^.:\\][=+\-*\/<>@$~&%|!?^.:\\]*)|(div)|(mod)|(shl)|(shr)/
        ),
        OP8: $ => /[+\-~|][+\-*\/<>@$~&%|!?^.:\\][=+\-*\/<>@$~&%|!?^.:\\]*/,
        OP7: $ => /[&][=+\-*\/<>@$~&%|!?^.:\\]*/,
        OP6: $ => /\.\.[=+\-*\/<>@$~&%|!?^.:\\]*/,
        OP5: $ => choice(
            'in',
            'notin',
            'is',
            'isnot',
            'of',
            'as',
            'from',
            /[=<>!][=+\-*\/<>@$~&%|!?^.:\\]*/
        ),
        OP4: $ => 'and',
        OP3: $ => choice('or', 'xor'),
        OP2: $ => /[@:?][=+\-*\/<>@$~&%|!?^.:\\]*/,
        OP1: $ => /[=+\-*\/<>@$~&%|!?^.:\\]+=/,
        OP0: $ => /[-=]+[>]+/,


        IND: $ => choice($.IND_EQ, $.IND_GE, $.DED),

        prefixOperator: $ => $.operator,
        simpleExpr: $ => choice(
            $.IDENT,
            $.binaryExpr
        ),


        binaryExpr: $ => choice(
            prec.left(9, seq($.binaryExpr, $.OP9, $.binaryExpr)),
            prec.left(8, seq($.binaryExpr, $.OP8, $.binaryExpr)),
            prec.left(7, seq($.binaryExpr, $.OP7, $.binaryExpr)),
            prec.left(6, seq($.binaryExpr, $.OP6, $.binaryExpr)),
            prec.left(5, seq($.binaryExpr, $.OP5, $.binaryExpr)),
            prec.left(4, seq($.binaryExpr, $.OP4, $.binaryExpr)),
            prec.left(3, seq($.binaryExpr, $.OP3, $.binaryExpr)),
            prec.left(2, seq($.binaryExpr, $.OP2, $.binaryExpr)),
            prec.left(1, seq($.binaryExpr, $.OP1, $.binaryExpr)),
            prec.left(0, seq($.binaryExpr, $.OP0, $.binaryExpr)),
        ),

        // symbol = '`' (KEYW|IDENT|literal|(operator|'('|')'|'['|']'|'{'|'}'|'=')+)+ '`'
        // | IDENT | KEYW
        symbol: $ => seq(
            '`',
            choice(
                $.KEYW,
                $.IDENT,
                $.literal,
                repeat1(choice(
                    $.operator,
                    '(', ')', '[', ']', '{', '}', '='))),
            '`'
        ),
        // exprColonEqExpr = expr (':'|'=' expr)?
        exprColonEqExpr: $ => seq($.expr, seq(choice(':', '='), $.expr)),

        // exprList = expr ^+ comma
        exprList: $ => sepList1($.expr, ','),

        // exprColonEqExprList = exprColonEqExpr (comma exprColonEqExpr)* (comma)?
        exprColonEqExprList: $ => seq(
            $.exprColonEqExpr,
            repeat(seq(',', $.exprColonEqExpr)),
            optional(',')
        ),

        // dotExpr = expr '.' optInd (symbol | '[:' exprList ']')
        dotExpr: $ => seq(
            $.expr,
            '.',
            optInd($),
            choice($.symbol, seq('[:', $.exprList, ']'))),

        // explicitGenericInstantiation = '[:' exprList ']' ( '(' exprColonEqExpr ')' )?
        explicitGenericInstantiation: $ => seq(
            '[:',
            $.exprList,
            ']',
            optional(seq('(', $.exprColonEqExpr, ')'))),

        // qualifiedIdent = symbol ('.' optInd symbol)?

        qualifiedIdent: $ => seq(
            $.symbol,
            optional(seq('.', optInd($), $.symbol))),

        // setOrTableConstr = '{' ((exprColonEqExpr comma)* | ':' ) '}'
        setOrTableConstr: $ => seq(
            '{',
            choice(seq($.exprColonEqExpr, ','), ':'),
            '}'
        ),

        // castExpr = 'cast' ('[' optInd typeDesc optPar ']' '(' optInd expr optPar ')')
        castExpr: $ => seq(
            'cast',
            seq('[', optInd($), $.typeDesc, optPar($), ']',
                '(', optInd($), $.expr, optPar($), ')')),

        // parKeyw = 'discard' | 'include' | 'if' | 'while' | 'case' | 'try'
        // | 'finally' | 'except' | 'for' | 'block' | 'const' | 'let'
        // | 'when' | 'var' | 'mixin'
        parKeyw: $ => seq(
            'discard', 'include', 'if', 'while', 'case', 'try', 'finally',
            'except', 'for', 'block', 'const', 'let', 'when', 'var',
            'mixin'
        ),

        // par = '(' optInd
        // ( &parKeyw (ifExpr \ complexOrSimpleStmt) ^+ ';'
        //   | ';' (ifExpr \ complexOrSimpleStmt) ^+ ';'
        //   | pragmaStmt
        //   | simpleExpr ( ('=' expr (';' (ifExpr \ complexOrSimpleStmt) ^+ ';' )? )
        //                  | (':' expr (',' exprColonEqExpr     ^+ ',' )? ) ) )
        // optPar ')'
        par: $ => seq(
            '(',
            optInd($),
            choice(
                seq(
                    $.parKeyw,
                    sepList1(choice($.ifExpr, $.complexOrSimpleStmt), ';')),

                seq(';', sepList1(choice($.ifExpr, $.complexOrSimpleStmt), ';')),

                $.pragmaStmt,
                $.simpleExpr,
                choice(
                    seq('=', $.expr, optional(seq(
                        ';', sepList1(choice($.ifExpr, $.complexOrSimpleStmt), ';')))),

                    seq(':', $.expr, optional(seq(
                        ',', sepList1($.exprColonEqExpr, ',')))))),
            optPar($),
            ')'
        ),

        // literal = | INT_LIT | INT8_LIT | INT16_LIT | INT32_LIT | INT64_LIT
        //     | UINT_LIT | UINT8_LIT | UINT16_LIT | UINT32_LIT | UINT64_LIT
        //     | FLOAT_LIT | FLOAT32_LIT | FLOAT64_LIT
        //     | STR_LIT | RSTR_LIT | TRIPLESTR_LIT
        //     | CHAR_LIT
        //     | NIL
        literal: $ => choice(
            $.INT_LIT,
            $.INT8_LIT,
            $.INT16_LIT,
            $.INT32_LIT,
            $.INT64_LIT,
            $.UINT_LIT,
            $.UINT8_LIT,
            $.UINT16_LIT,
            $.UINT32_LIT,
            $.UINT64_LIT,
            $.FLOAT_LIT,
            $.FLOAT32_LIT,
            $.FLOAT64_LIT,
            $.STR_LIT,
            $.RSTR_LIT,
            $.TRIPLESTR_LIT,
            $.CHAR_LIT,
            $.NIL
        ),

        generalizedLit: $ => choice($.GENERALIZED_STR_LIT, $.GENERALIZED_TRIPLESTR_LIT),
        // identOrLiteral = generalizedLit | symbol | literal
        //     | par | arrayConstr | setOrTableConstr
        //     | castExpr

        identOrLiteral: $ => choice(
            $.generalizedLit,
            $.symbol,
            $.literal,
            $.par,
            $.arrayConstr,
            $.setOrTableConstr,
            $.castExpr
        ),

        // tupleConstr = '(' optInd (exprColonEqExpr comma?)* optPar ')'
        tupleConstr: $ => seq(
            '(',
            optInd($),
            repeat(seq($.exprColonEqExpr, optional(','))),
            optPar($),
            ')'
        ),

        // arrayConstr = '[' optInd (exprColonEqExpr comma?)* optPar ']'
        arrayConstr: $ => prec(2, seq(
            '[',
            optInd($),
            repeat(seq($.exprColonEqExpr, optional(','))),
            optPar($),
            ']'
        )),

        // primarySuffix = '(' (exprColonEqExpr comma?)* ')'
        //     | '.' optInd symbol generalizedLit?
        //     | '[' optInd exprColonEqExprList optPar ']'
        //     | '{' optInd exprColonEqExprList optPar '}'
        //     | &( '`'|IDENT|literal|'cast'|'addr'|'type') expr # command syntax

        primarySuffix: $ => choice(
            seq('(', repeat(seq($.exprColonEqExpr, optional(','))), ')'),
            seq('.', optInd($), $.symbol, optional($.generalizedLit)),
            seq('[', optInd($), $.exprColonEqExprList, optPar($), ']'),
            seq('{', optInd($), $.exprColonEqExprList, optPar($), '}'),
            seq(choice('`', $.IDENT, $.literal, 'cast', 'addr', 'type'), $.expr)
        ),

        pragma: $ => prec(2, seq(
            '{.',
            optInd($),
            repeat(seq($.exprColonEqExpr, optional(','))),
            optPar($),
            choice('.}', '}')
        )),

        // identVis = symbol OPR?  # postfix position
        identVis: $ => seq($.symbol, optional($.OPR)),

        // identVisDot = symbol '.' optInd symbol OPR?
        identVisDot: $ => seq($.symbol, '.', optInd($), $.symbol, optional($.OPR)),

        // identWithPragma = identVis pragma?
        identWithPragma: $ => seq($.identVis, optional($.pragma)),

        // identWithPragmaDot = identVisDot pragma?
        identWithPragmaDot: $ => seq($.identVisDot, optional($.pragma)),

        // declColonEquals = identWithPragma (comma identWithPragma)* comma?
        //   (':' optInd typeDesc)? ('=' optInd expr)?
        declColonEquals: $ => prec.left(seq(
            $.identWithPragma,
            repeat(seq(',', $.identWithPragma)),
            optional(','),
            optional(seq(':', optInd($), $.typeDesc)),
            optional(seq('=', optInd($), $.expr))
        )),

        // identColonEquals = IDENT (comma IDENT)* comma?
        //   (':' optInd typeDesc)? ('=' optInd expr)?)
        identColonEquals: $ => prec.left(2, seq(
            $.IDENT,
            repeat(seq(',', $.IDENT)),
            optional(','),
            optional(seq(':', optInd($), $.typeDesc)),
            optional(seq('=', optInd($), $.expr))
        )),

        // inlTupleDecl = 'tuple'
        //   '[' optInd  (identColonEquals (comma/semicolon)?)*  optPar ']'
        inlTupleDecl: $ => seq(
            'tuple',
            '[',
            optInd($),
            repeat(seq($.identColonEquals, /,;/)),
            optPar($),
            ']'
        ),

        // extTupleDecl = 'tuple'
        //   COMMENT? (IND{>} identColonEquals (IND{=} identColonEquals)*)?
        extTupleDecl: $ => seq(
            'tuple',
            seq(
                $.IND_GE,
                $.identColonEquals,
                repeat(seq($.IND_EQ, $.identColonEquals))
            )
        ),

        // tupleClass = 'tuple'
        tupleClass: $ => 'tuple',

        // paramList = '(' declColonEquals ^* (comma/semicolon) ')'
        paramList: $ => seq('(', sepList($.declColonEquals, /,;/), ')'),

        // doBlock = 'do' paramListArrow pragma? colcom stmt
        doBlock: $ => seq(
            'do', paramListArrow($), optional($.pragma), ':',
            $.stmt
        ),

        // procExpr = 'proc' paramListColon pragma? ('=' COMMENT? stmt)?
        procExpr: $ => seq(
            'proc',
            paramListColon($),
            optional($.pragma),
            optional(seq('=', $.stmt))
        ),

        // distinct = 'distinct' optInd typeDesc
        distinct: $ => seq('distinct', optInd($), $.typeDesc),
        // forStmt = 'for' (identWithPragma ^+ comma) 'in' expr colcom stmt
        forStmt: $ => seq(
            'for',
            sepList1($.identWithPragma, ','),
            prec(2, 'in'),
            $.expr,
            ':',
            $.stmt
        ),

        // forExpr = forStmt
        forExpr: $ => $.forStmt,

        // expr = (blockExpr
        //         | ifExpr
        //         | whenExpr
        //         | caseStmt
        //         | forExpr
        //         | tryExpr)
        //     / simpleExpr
        expr: $ => prec(2, choice(
            choice(
              $.blockExpr,
              $.ifExpr,
              $.whenExpr,
              $.caseStmt,
              $.forExpr,
              $.tryExpr
            ),
            $.simpleExpr
        )),

        // typeKeyw = 'var' | 'out' | 'ref' | 'ptr' | 'shared' | 'tuple'
        //     | 'proc' | 'iterator' | 'distinct' | 'object' | 'enum'
        typeKeyw: $ => prec(2, choice(
            'var',
            'out',
            'ref',
            'ptr',
            'shared',
            'tuple',
            'proc',
            'iterator',
            'distinct',
            'object',
            'enum'
        )),

        // primary = typeKeyw optInd typeDesc
        //     /  prefixOperator* identOrLiteral primarySuffix*

        primary: $ => seq(
            seq($.typeKeyw, optInd($), $.typeDesc),
            seq(repeat($.prefixOperator), $.identOrLiteral, repeat($.primarySuffix)),
            seq('bind', $.primary)
        ),

        // typeDesc = simpleExpr ('not' expr)?
        typeDesc: $ => seq($.simpleExpr, optional(seq('not', $.expr))),

        // typeDefAux = simpleExpr ('not' expr)?
        //            | 'concept' typeClass

        typeDefAux: $ => choice(
            seq($.simpleExpr, optional(seq('not', $.expr))),
            seq('concept', $.typeClass)
        ),

        // postExprBlocks = ':' stmt? ( IND{=} doBlock
        //                            | IND{=} 'of' exprList ':' stmt
        //                            | IND{=} 'elif' expr ':' stmt
        //                            | IND{=} 'except' exprList ':' stmt
        //                            | IND{=} 'else' ':' stmt )*

        postExprBlocks: $ => prec.left(seq(
            ':',
            optional($.stmt),
            repeat(
                choice(
                    seq($.IND_EQ, $.doBlock),
                    seq($.IND_EQ, 'of', $.exprList, ':', $.stmt),
                    seq($.IND_EQ, 'elif', $.expr, ':', $.stmt),
                    seq($.IND_EQ, 'except', $.exprList, ':', $.stmt),
                    seq($.IND_EQ, 'else', ':', $.stmt),
                )
            )
        )),


        // exprStmt = simpleExpr
        //          (( '=' optInd expr colonBody? )
        //          / ( expr ^+ comma
        //              postExprBlocks
        //            ))?

        exprStmt: $ => prec.left(seq(
            $.simpleExpr,
            optional(
                choice(
                    seq('=', optInd($), $.expr, optional($.colonBody)),
                    seq(sepList1($.expr, ','), $.postExprBlocks)
                )
            )
        )),

        // importStmt = 'import' optInd expr
        //               ((comma expr)*
        //               / 'except' optInd (expr ^+ comma))

        importStmt: $ => prec.left(seq(
            'import',
            optInd($),
            $.expr,
            choice(
                repeat(seq(',', $.expr)),
                seq('except', optInd($), sepList1($.expr, ','))
            )
        )),

        // exportStmt = 'export' optInd expr
        //               ((comma expr)*
        //               / 'except' optInd (expr ^+ comma))

        exportStmt: $ => prec.left(seq(
            'export',
            optInd($),
            $.expr,
            choice(
                repeat(seq(',', $.expr)),
                seq('except', optInd($), sepList1($.expr, ','))
            )
        )),

        // includeStmt = 'include' optInd expr ^+ comma
        includeStmt: $ => prec.left(
            seq('include', optInd($), sepList1($.expr, ','))),

        // fromStmt = 'from' expr 'import' optInd expr (comma expr)*
        fromStmt: $ => prec.left(seq(
            'from',
            $.expr,
            'import',
            optInd($),
            $.expr,
            repeat(seq(',', $.expr))
        )),

        // returnStmt = 'return' optInd expr?
        returnStmt: $ => prec.left(
            seq('return', optInd($), optional($.expr))),

        // raiseStmt = 'raise' optInd expr?
        raiseStmt: $ => prec.left(
            seq('raise', optInd($), optional($.expr))),

        // yieldStmt = 'yield' optInd expr?
        yieldStmt: $ => prec.left(
            seq('yield', optInd($), optional($.expr))),

        // discardStmt = 'discard' optInd expr?
        discardStmt: $ => prec.left(
            seq('discard', optInd($), optional($.expr))),

        // breakStmt = 'break' optInd expr?
        breakStmt: $ => prec.left(
            seq('break', optInd($), optional($.expr))),

        // continueStmt = 'break' optInd expr?
        continueStmt: $ => prec.left(
            seq('continue', optInd($), optional($.expr))),

        // condStmt = expr colcom stmt COMMENT?
        //            (IND{=} 'elif' expr colcom stmt)*
        //            (IND{=} 'else' colcom stmt)?

        condStmt: $ => prec.left(seq(
            seq($.expr, ':', $.stmt),
            repeat(seq($.IND_EQ, 'elif', $.expr, ':', $.stmt)),
            optional(seq($.IND_EQ, 'else', ':', $.stmt))
        )),

        // ifStmt = 'if' condStmt
        ifStmt: $ => seq('if', $.condStmt),

        // whenStmt = 'when' condStmt
        whenStmt: $ => seq('when', $.condStmt),

        // condExpr = expr colcom expr optInd
        //         ('elif' expr colcom expr optInd)*
        //          'else' colcom expr
        condExpr: $ => seq(
            seq($.expr, ':', $.expr, optInd($)),
            repeat(seq('elif', $.expr, ':', $.expr, optInd($))),
            seq('else', ':', $.expr)
        ),


        // ifExpr = 'if' condExpr
        ifExpr: $ => seq('if', $.condExpr),

        // whenExpr = 'when' condExpr
        whenExpr: $ => seq('when', $.condExpr),

        // whileStmt = 'while' expr colcom stmt
        whileStmt: $ => seq('while', $.expr, ':', $.stmt),

        // ofBranch = 'of' exprList colcom stmt
        ofBranch: $ => seq('of', $.exprList, ':', $.stmt),

        // ofBranch = 'of' exprList colcom stmt
        ofBranch: $ => seq('of', $.exprList, ':', $.stmt),

        // ofBranches = ofBranch (IND{=} ofBranch)*
        //                       (IND{=} 'elif' expr colcom stmt)*
        //                       (IND{=} 'else' colcom stmt)?
        ofBranches: $ => prec.left(seq(
            $.ofBranch,
            repeat(seq($.IND_EQ, $.ofBranch)),
            repeat(seq($.IND_EQ, 'elif', $.expr, ':', $.stmt)),
            optional(seq($.IND_EQ, 'else', ':', $.stmt))
        )),

        // caseStmt = 'case' expr ':'? COMMENT?
        //             (IND{>} ofBranches DED
        //             | IND{=} ofBranches)
        caseStmt: $ => seq(
            seq('case', $.expr, optional(':')),
            seq($.IND_GE, $.ofBranches, $.DED, $.IND_EQ, $.ofBranches)),

        // tryStmt = 'try' colcom stmt &(IND{=}? 'except'|'finally')
        //            (IND{=}? 'except' exprList colcom stmt)*
        //            (IND{=}? 'finally' colcom stmt)?

        tryStmt: $ => prec.left(seq(
            'try', ':', $.stmt,
            seq(optional($.IND_EQ), 'except', 'finally'),
            repeat(seq(optional($.IND_EQ), 'except',  $.exprList, ':', $.stmt)),
            optional(seq(optional($.IND_EQ), 'finally', ':', $.stmt))
        )),

        // tryExpr = 'try' colcom stmt &(optInd 'except'|'finally')
        //            (optInd 'except' exprList colcom stmt)*
        //            (optInd 'finally' colcom stmt)?
        tryExpr: $ => prec.left(seq(
            'try', ':', $.stmt, seq(optInd($), choice('except', 'finally')),
            repeat(seq(optInd($), 'except', $.exprList, ':', $.stmt)),
            optional(seq(optInd($), 'finally', ':', $.stmt))
        )),

        // exceptBlock = 'except' colcom stmt
        exceptBlock: $ => seq('except', ':', $.stmt),

        // blockStmt = 'block' symbol? colcom stmt
        blockStmt: $ => seq('block', optional($.symbol), ':', $.stmt),

        // blockExpr = 'block' symbol? colcom stmt
        blockExpr: $ => seq('block', optional($.symbol), ':', $.stmt),

        // staticStmt = 'static' colcom stmt
        staticStmt: $ => seq('static', ':', $.stmt),

        // deferStmt = 'defer' colcom stmt
        deferStmt: $ => seq('defer', ':', $.stmt),

        // asmStmt = 'asm' pragma? (STR_LIT | RSTR_LIT | TRIPLESTR_LIT)
        asmStmt: $ => seq(
            'asm',
            optional($.pragma),
            choice($.STR_LIT, $.RSTR_LIT, $.TRIPLESTR_LIT)
        ),

        // genericParam = symbol (comma symbol)* (colon expr)? ('=' optInd expr)?
        genericParam: $ => prec.left(seq(
            $.symbol,
            repeat(seq(',', $.symbol)),
            optional(seq(':', $.expr)),
            optional(seq('=', optInd($), $.expr))
        )),

        // genericParamList = '[' optInd
        //   genericParam ^* (comma/semicolon) optPar ']'
        genericParamList: $ => seq(
            '[',
            optInd($),
            sepList($.genericParam, /,;/),
            optPar($),
            ']'
        ),

        // pattern = '{' stmt '}'
        pattern: $ => seq('{', $.stmt, '}'),


        // routine = optInd identVis pattern? genericParamList?
        //   paramListColon pragma? ('=' COMMENT? stmt)? indAndComment

        routine: $ => prec.left(seq(
            optInd($),
            $.identVis,
            optional($.pattern),
            optional($.genericParamList),
            paramListColon($),
            optional($.pragma),
            optional(seq('=', $.stmt)),
            indAndComment($)
        )),

        // commentStmt = COMMENT
        commentStmt: $ => prec(2, $.COMMENT),

        // enum = 'enum' optInd (symbol pragma? optInd ('=' optInd expr COMMENT?)? comma?)+
        enum: $ => seq(
            'enum',
            optInd($),
            repeat1(seq(
                $.symbol,
                optional($.pragma),
                optInd($),
                optional(seq('=', optInd($), $.expr)),
                optional(',')
            ))
        ),

        // objectWhen = 'when' expr colcom objectPart COMMENT?
        //             ('elif' expr colcom objectPart COMMENT?)*
        //             ('else' colcom objectPart COMMENT?)?
        objectWhen: $ => seq(
            'when',
            seq($.expr, ':', $.objectPart),
            repeat(seq('elif', $.expr, ':', $.objectPart)),
            optional(seq('else', ':', $.objectPart))
        ),

        // objectBranch = 'of' exprList colcom objectPart
        objectBranch: $ => seq('of', $.exprList, ':', $.objectPart),

        // objectBranches = objectBranch (IND{=} objectBranch)*
        //                       (IND{=} 'elif' expr colcom objectPart)*
        //                       (IND{=} 'else' colcom objectPart)?
        objectBranches: $ => seq(
            $.objectBranch,
            repeat(seq($.IND_EQ, $.objectBranch)),
            repeat(seq($.IND_EQ, 'elif', $.expr, ':', $.objectPart)),
            optional(seq($.IND_EQ, 'else', ':', $.objectPart))
        ),


        // objectCase = 'case' identWithPragma ':' typeDesc ':'? COMMENT?
        //             (IND{>} objectBranches DED
        //             | IND{=} objectBranches)
        objectCase: $ => seq(
            'case', $.identWithPragma, ':', $.typeDesc, optional(':'),
            choice(
                seq($.IND_GE, $.objectBranches, $.DED),
                seq($.IND_EQ, $.objectBranches)
            )
        ),


        // objectPart = IND{>} objectPart^+IND{=} DED
        //            / objectWhen / objectCase / 'nil' / 'discard' / declColonEquals
        objectPart: $ => choice(
            seq($.IND_GE,  sepList1($.objectPart, $.IND_EQ), $.DED),
            $.objectWhen,
            $.objectCase,
            'nil',
            'discard',
            $.declColonEquals
        ),

        // object = 'object' pragma? ('of' typeDesc)? COMMENT? objectPart
        object: $ => seq(
            'object',
            optional($.pragma),
            optional(seq('of', $.typeDesc)),
            $.objectPart
        ),

        // typeClassParam = ('var' | 'out')? symbol
        typeClassParam: $ => seq(optional(choice('var', 'out')), $.symbol),

        // typeClass = typeClassParam ^* ',' (pragma)? ('of' typeDesc ^* ',')?
        //               &IND{>} stmt
        typeClass: $ => seq(
            sepList($.typeClassParam, ','),
            optional($.pragma),
            optional(seq('of', sepList($.typeDesc, ','))),
            $.IND_GE,
            $.stmt
        ),

        // typeDef = identWithPragmaDot genericParamList? '=' optInd typeDefAux
        //             indAndComment? / identVisDot genericParamList? pragma '=' optInd typeDefAux
        //             indAndComment?

        // WARNING FIXME ERROR NOTE FUCKING_SHIT
        typeDef: $ => prec.left(seq(
            $.identWithPragmaDot,
            optional($.genericParamList),
            '=',
            optInd($),
            $.typeDefAux,
            choice(optional(indAndComment($)), $.identVisDot),
            optional($.genericParamList),
            $.pragma,
            '=',
            optInd($),
            $.typeDefAux,
            optional(indAndComment($))
        )),

        // varTuple = '(' optInd identWithPragma ^+ comma optPar ')' '=' optInd expr
        varTuple: $ => seq(
            '(',
            optInd($),
            sepList1($.identWithPragma, ','),
            optPar($),
            ')',
            '=',
            optInd($),
            $.expr
        ),

        // colonBody = colcom stmt postExprBlocks?
        colonBody: $ => prec.left(
            seq(':', $.stmt, optional($.postExprBlocks))),

        // variable = (varTuple / identColonEquals) colonBody? indAndComment
        variable: $ => prec.left(seq(
            choice($.varTuple, $.identColonEquals),
            optional($.colonBody),
            indAndComment($)
        )),

        // constant = (varTuple / identWithPragma) (colon typeDesc)? '=' optInd expr indAndComment
        constant: $ => prec.left(seq(
            choice($.varTuple, $.identWithPragma),
            optional(seq(':', $.typeDesc)),
            '=',
            optInd($),
            $.expr,
            indAndComment($)
        )),

        // bindStmt = 'bind' optInd qualifiedIdent ^+ comma
        bindStmt: $ => seq('bind', optInd($), sepList1($.qualifiedIdent, ',')),

        // mixinStmt = 'mixin' optInd qualifiedIdent ^+ comma
        mixinStmt: $ => seq('mixin', optInd($), sepList1($.qualifiedIdent, ',')),

        // pragmaStmt = pragma (':' COMMENT? stmt)?
        pragmaStmt: $ => prec.left(
            seq($.pragma, optional(seq(':', $.stmt)))),

        // simpleStmt = ((returnStmt | raiseStmt | yieldStmt | discardStmt | breakStmt
        //            | continueStmt | pragmaStmt | importStmt | exportStmt | fromStmt
        //            | includeStmt | commentStmt) / exprStmt) COMMENT?
        simpleStmt: $ => prec.left(seq(
            choice(
                choice(
                    $.returnStmt,
                    $.raiseStmt,
                    $.yieldStmt,
                    $.discardStmt,
                    $.breakStmt,
                    $.continueStmt,
                    $.pragmaStmt,
                    $.importStmt,
                    $.exportStmt,
                    $.fromStmt,
                    $.includeStmt,
                    $.commentStmt,
                ),
                $.exprStmt
            ),
        )),

        // complexOrSimpleStmt = (ifStmt | whenStmt | whileStmt
        //                     | tryStmt | forStmt
        //                     | blockStmt | staticStmt | deferStmt | asmStmt
        //                     | 'proc' routine
        //                     | 'method' routine
        //                     | 'func' routine
        //                     | 'iterator' routine
        //                     | 'macro' routine
        //                     | 'template' routine
        //                     | 'converter' routine
        //                     | 'type' section(typeDef)
        //                     | 'const' section(constant)
        //                     | ('let' | 'var' | 'using') section(variable)
        //                     | bindStmt | mixinStmt)
        //                     / simpleStmt
        complexOrSimpleStmt: $ => choice(
            choice(
                $.ifStmt,
                $.whenStmt,
                $.whileStmt,
                $.tryStmt,
                $.forStmt,
                $.blockStmt,
                $.staticStmt,
                $.deferStmt,
                $.asmStmt,
                seq('proc', $.routine),
                seq('method', $.routine),
                seq('func', $.routine),
                seq('iterator', $.routine),
                seq('macro', $.routine),
                seq('template', $.routine),
                seq('converter', $.routine),
                seq('type', section($, $.typeDef)),
                seq('const', section($, $.constant)),
                seq(choice('let', 'var', 'using'), section($, $.variable)),
                $.bindStmt,
                $.mixinStmt
            ),
            $.simpleStmt
        ),

        // stmt = (IND{>} complexOrSimpleStmt^+(IND{=} / ';') DED)
        //      / simpleStmt ^+ ';'
        stmt: $ => choice(
            prec(2, seq(
                $.IND_GE,
                sepList1(
                    $.complexOrSimpleStmt,
                    choice($.IND_EQ, ';')),
                $.DED
            )),
            prec(1, sepList1($.simpleStmt, ';'))
        ),


        // NIL
        NIL: $ => 'nil',

        // // hexdigit = digit | 'A'..'F' | 'a'..'f'
        // hexdigit: $ => /[0-9A-Fa-f]/,

        // // octdigit = '0'..'7'
        // octdigit: $ => /[0-7]/,

        // // bindigit = '0'..'1'
        // bindigit: $ => '0'..'1',

        // HEX_LIT = '0' ('x' | 'X' ) hexdigit ( ['_'] hexdigit )*
        HEX_LIT: $ => /0(x|X)[0-9A-Fa-f]([_0-9A-Fa-f])*/,

        // DEC_LIT = digit ( ['_'] digit )*
        DEC_LIT: $ => /[0-9]([_0-9])*/,

        // OCT_LIT = '0' 'o' octdigit ( ['_'] octdigit )*
        OCT_LIT: $ => /0o[0-7]([_0-7])*/,

        // BIN_LIT = '0' ('b' | 'B' ) bindigit ( ['_'] bindigit )*
        BIN_LIT: $ => /0b[01]([_01])*/,

        // INT_LIT = HEX_LIT
        //         | DEC_LIT
        //         | OCT_LIT
        //         | BIN_LIT
        INT_LIT: $ => seq($.HEX_LIT, $.DEC_LIT, $.OCT_LIT, $.BIN_LIT),

        // INT8_LIT = INT_LIT ['\''] ('i' | 'I') '8'
        INT8_LIT: $ => seq($.INT_LIT, /\'(i|I)8/),

        // INT16_LIT = INT_LIT ['\''] ('i' | 'I') '16'
        INT16_LIT: $ => seq($.INT_LIT, /\'(i|I)16/),

        // INT32_LIT = INT_LIT ['\''] ('i' | 'I') '32'
        INT32_LIT: $ => seq($.INT_LIT, /\'(i|I)32/),

        // INT64_LIT = INT_LIT ['\''] ('i' | 'I') '64'
        INT64_LIT: $ => seq($.INT_LIT, /\'(i|I)64/),

        // UINT_LIT = INT_LIT ['\''] ('u' | 'U')
        UINT_LIT: $ => seq($.INT_LIT, /\'u/),

        // UINT8_LIT = INT_LIT ['\''] ('u' | 'U') '8'
        UINT8_LIT: $ => seq($.INT_LIT, /\'(u|U)8/),

        // UINT16_LIT = INT_LIT ['\''] ('u' | 'U') '16'
        UINT16_LIT: $ => seq($.INT_LIT, /\'(u|U)16/),

        // UINT32_LIT = INT_LIT ['\''] ('u' | 'U') '32'
        UINT32_LIT: $ => seq($.INT_LIT, /\'(u|U)32/),

        // UINT64_LIT = INT_LIT ['\''] ('u' | 'U') '64'
        UINT64_LIT: $ => seq($.INT_LIT, /\'(u|U)64/),

        // exponent = ('e' | 'E' ) ['+' | '-'] digit ( ['_'] digit )*
        exponent: $ => /[eE][+-][0-9][_0-9]*/,

        // FLOAT_LIT = digit (['_'] digit)* (('.' digit (['_'] digit)* [exponent]) |exponent)
        FLOAT_LIT: $ => /[0-9][_0-9]*(\.[0-9][_0-9]*[eE][+-][0-9][_0-9]*)|([eE][+-][0-9][_0-9]*)/,

        // FLOAT32_SUFFIX = ('f' | 'F') ['32']
        FLOAT32_SUFFIX: $ => /[fF]32/,

        // FLOAT32_LIT = HEX_LIT '\'' FLOAT32_SUFFIX
        //             | (FLOAT_LIT | DEC_LIT | OCT_LIT | BIN_LIT) ['\''] FLOAT32_SUFFIX
        FLOAT32_LIT: $ => choice(
            seq($.HEX_LIT, '\'', $.FLOAT32_SUFFIX),
            seq(choice($.FLOAT_LIT, $.DEC_LIT, $.OCT_LIT, $.BIN_LIT),
                '\'', $.FLOAT32_SUFFIX)
        ),

        // FLOAT64_SUFFIX = ( ('f' | 'F') '64' ) | 'd' | 'D'
        FLOAT64_SUFFIX: $ => /([fF]64|d|D)/,

        // FLOAT64_LIT = HEX_LIT '\'' FLOAT64_SUFFIX
        //             | (FLOAT_LIT | DEC_LIT | OCT_LIT | BIN_LIT) ['\''] FLOAT64_SUFFIX
        FLOAT64_LIT: $ => choice(
            seq($.HEX_LIT, '\'', $.FLOAT64_SUFFIX),
            seq(choice($.FLOAT_LIT, $.DEC_LIT, $.OCT_LIT, $.BIN_LIT),
                '\'', $.FLOAT64_SUFFIX)
        ),
    }
});
