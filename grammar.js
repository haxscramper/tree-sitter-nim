function sepList(rule, separator) {
    return seq(rule, repeat(seq(separator, rule)));
}

function sepList1(rule, separator) {
    return seq(rule, repeat1(seq(separator, rule)));
}

function comm() {
    return optional($.COMMENT);
}

{
    rules: {
        // WARNING
        // module = stmt ^* (';' / IND{=})
        module: $ => sepList($.stmt, choice(';', $.indent)),

        // comma = ',' COMMENT?
        comma: $ => seq(',', comm()),

        // semicolon = ';' COMMENT?
        semicolon: $ => seq(';', comm()),

        // colon = ':' COMMENT?
        colon: $ => seq(':', comm()),

        // colcom = ':' COMMENT?
        colcom: $ => seq(':', comm()),

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
            $.OP9,
            'or',
            'xor',
            'and',
            'is',
            'isnot',
            'in',
            'notin',
            'of',
            'as',
            'from',
            'div',
            'mod',
            'shl',
            'shr',
            'not',
            'static',
            '..'
        ),



        // prefixOperator = operator
        prefixOperator: $ => operator,

        // optInd = COMMENT? IND?
        optInd: $ => seq(comm(), optional($.IND)),

        // optPar = (IND{>} | IND{=})?
        optPar: $ => optional(choice($.IND_GE, $.IND_EQ)),

        // simpleExpr = arrowExpr (OP0 optInd arrowExpr)* pragma?
        simpleExpr: $ => seq(
            $.arrowExpr,
            repeat(seq($.OP0, $.optInd, $.arrowExpr)),
            optional($.pragma)
        ),

        // arrowExpr = assignExpr (OP1 optInd assignExpr)*
        arrowExpr: $ => seq(
            $.assignExpr,
            repeat(seq($.OP1, $.optInd, $.assignExpr))),

        // assignExpr = orExpr (OP2 optInd orExpr)*
        assignExpr: $ => seq(
            $.orExpr,
            repeat(seq($.OP2, $.optInd, $.orExpr))),

        // orExpr = andExpr (OP3 optInd andExpr)*
        orExpr: $ => seq(
            $.andExpr,
            repeat(seq($.OP3, $.optInd, $.andExpr))),

        // andExpr = cmpExpr (OP4 optInd cmpExpr)*
        andExpr: $ => seq(
            $.cmpExpr,
            repeat(seq($.OP4, $.optInd, $.cmpExpr))),

        // cmpExpr = sliceExpr (OP5 optInd sliceExpr)*
        cmpExpr: $ => seq(
            $.sliceExpr,
            repeat(seq($.OP5, $.optInd, $.sliceExpr))),

        // ampExpr = plusExpr (OP7 optInd plusExpr)*
        ampExpr: $ => seq(
            $.plusExpr,
            repeat(seq($.OP7, $.optInd, $.plusExpr))),

        // plusExpr = mulExpr (OP8 optInd mulExpr)*
        plusExpr: $ => seq(
            $.mulExpr,
            repeat(seq($.OP8, $.optInd, $.mulExpr))),

        // mulExpr = dollarExpr (OP9 optInd dollarExpr)*
        mulExpr: $ => seq(
            $.dollarExpr,
            repeat(seq($.OP9, $.optInd, $.dollarExpr))),

        // dollarExpr = primary (OP10 optInd primary)*
        dollarExpr: $ => seq($.primary, repeat(seq($.OP10, $.optInd, $primary))),

        // symbol = '`' (KEYW|IDENT|literal|(operator|'('|')'|'['|']'|'{'|'}'|'=')+)+ '`'
        // | IDENT | KEYW

        symbol: $ => seq(
            '`',
            choice(
                $.KEYW,
                $.IDENT,
                $.literal,
                repeat1(choise(
                    $.operator,
                    '(', ')', '[', ']', '{', '}', '='))),
            '`'
        ),
        // exprColonEqExpr = expr (':'|'=' expr)?
        exprColonEqExpr: $ => seq($.expr, seq(choice(':', '='), $.expr)),

        // exprList = expr ^+ comma
        epxrList: $ => setList1($.expr, $.comma),

        // exprColonEqExprList = exprColonEqExpr (comma exprColonEqExpr)* (comma)?
        exprColonEqExprList: $ => seq(
            $.exprColonEqExpr,
            repeat(seq($.comma, $.exprColonEqExpr)),
            optional($.comma)
        ),

        // dotExpr = expr '.' optInd (symbol | '[:' exprList ']')
        dotExpr: $ => seq(
            $.expr,
            '.',
            $.optInd,
            choice($.symbol, seq('[:', exprList, ']'))),

        // explicitGenericInstantiation = '[:' exprList ']' ( '(' exprColonEqExpr ')' )?
        explicitGenericInstantiation: $ => seq(
            '[:',
            $.exprList,
            ']',
            optional('(', $.exprColonEqExpr, ')')),

        // qualifiedIdent = symbol ('.' optInd symbol)?

        qualifiedIdent: $ => seq(
            $.symbol,
            optional(seq('.', $.optInd, $.symbol))),

        // setOrTableConstr = '{' ((exprColonEqExpr comma)* | ':' ) '}'
        setOrTableConstr: $ => seq(
            '{',
            choice(seq($.exprColonEqExpr, $.comma), ':'),
            '}'
        ),

        // castExpr = 'cast' ('[' optInd typeDesc optPar ']' '(' optInd expr optPar ')')
        castExpr: $ => seq(
            'cast',
            seq('[', $.optInd, $.typeDesc, $.optPar, ']',
                '(', $.optInd, $.expr, $.optPar, ')')),

        // parKeyw = 'discard' | 'include' | 'if' | 'while' | 'case' | 'try'
        // | 'finally' | 'except' | 'for' | 'block' | 'const' | 'let'
        // | 'when' | 'var' | 'mixin'
        parKeyw: $ => seq(
            'discard',
            'include',
            'if',
            'while',
            'case',
            'try',
            'finally',
            'except',
            'for',
            'block',
            'const',
            'let',
            'when',
            'var',
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
            $.optInd,
            choice(
                seq(
                    $.parKeyw,
                    sepList1(choice(ifExpr, complexOrSimpleStmt), ';')),

                seq(';', sepList1(choice($.ifExpr, $.complexOrSimpleStmt), ';')),

                $.pragmaStmt,
                $.simpleExpr,
                choice(
                    seq('=', $.expr, optional(seq(
                        ';', sepList1(choice($.ifExpr, $.complexOrSimpleStmt), ';')))),

                    seq(':', $.expr, optional(seq(
                        ',', sepList1($.exprColonEqExpr, ',')))))),
            $.optPar,
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
            $.optInd,
            repeat(seq($.exprColonEqExpr, optional($.comma))),
            $.optPar,
            ')'
        ),

        // arrayConstr = '[' optInd (exprColonEqExpr comma?)* optPar ']'
        arrayConstr: $ => seq(
            '[',
            $.optInd,
            repeat($.exprColonEqExpr, optional(comma)),
            $.optPar,
            ']'
        ),

        // primarySuffix = '(' (exprColonEqExpr comma?)* ')'
        //     | '.' optInd symbol generalizedLit?
        //     | '[' optInd exprColonEqExprList optPar ']'
        //     | '{' optInd exprColonEqExprList optPar '}'
        //     | &( '`'|IDENT|literal|'cast'|'addr'|'type') expr # command syntax

        primarySuffix: $ => choice(
            seq('(', repeat(seq($.exprColonEqExpr, optional($.comma))) ')'),
            seq('.', $.optInd, $.symbol, $.optional($.generalizedLit)),
            seq('[', $.optInd, $.exprColonEqExprList, $.optPar, ']'),
            seq('{', $.optInd, $.exprColonEqExprList, $.optPar, '}'),
            seq(choice('`', $.IDENT, $.literal, 'cast', 'addr', 'type'), $.expr)
        ),

        pragma: $ => seq(
            '{.',
            $.optInd,
            repeat($.exprColonEqExpr, optional($.comma)),
            $.optPar,
            choice('.}', '}')
        ),

        // identVis = symbol OPR?  # postfix position
        identVis: $ => seq($.symbol, optional($.OPR)),

        // identVisDot = symbol '.' optInd symbol OPR?
        identVisDot: $ => seq($.symbol, '.', $.optInd, $.symbol, optional($.OPR)),

//         identWithPragma = identVis pragma?
//         identWithPragmaDot = identVisDot pragma?
//         declColonEquals = identWithPragma (comma identWithPragma)* comma?
//         (':' optInd typeDesc)? ('=' optInd expr)?
//         identColonEquals = IDENT (comma IDENT)* comma?
//         (':' optInd typeDesc)? ('=' optInd expr)?)
// inlTupleDecl = 'tuple'
// '[' optInd  (identColonEquals (comma/semicolon)?)*  optPar ']'
// extTupleDecl = 'tuple'
// COMMENT? (IND{>} identColonEquals (IND{=} identColonEquals)*)?
//     tupleClass = 'tuple'
// paramList = '(' declColonEquals ^* (comma/semicolon) ')'
// paramListArrow = paramList? ('->' optInd typeDesc)?
//     paramListColon = paramList? (':' optInd typeDesc)?
//     doBlock = 'do' paramListArrow pragma? colcom stmt
// procExpr = 'proc' paramListColon pragma? ('=' COMMENT? stmt)?
//     distinct = 'distinct' optInd typeDesc
// forStmt = 'for' (identWithPragma ^+ comma) 'in' expr colcom stmt
// forExpr = forStmt
// expr = (blockExpr
//         | ifExpr
//         | whenExpr
//         | caseStmt
//         | forExpr
//         | tryExpr)
//     / simpleExpr
// typeKeyw = 'var' | 'out' | 'ref' | 'ptr' | 'shared' | 'tuple'
//     | 'proc' | 'iterator' | 'distinct' | 'object' | 'enum'
// primary = typeKeyw optInd typeDesc
//     /  prefixOperator* identOrLiteral primarySuffix*
// / 'bind' primary
// typeDesc = simpleExpr ('not' expr)?
// typeDefAux = simpleExpr ('not' expr)?
//            | 'concept' typeClass
// postExprBlocks = ':' stmt? ( IND{=} doBlock
//                            | IND{=} 'of' exprList ':' stmt
//                            | IND{=} 'elif' expr ':' stmt
//                            | IND{=} 'except' exprList ':' stmt
//                            | IND{=} 'else' ':' stmt )*
// exprStmt = simpleExpr
//          (( '=' optInd expr colonBody? )
//          / ( expr ^+ comma
//              postExprBlocks
//            ))?
// importStmt = 'import' optInd expr
//               ((comma expr)*
//               / 'except' optInd (expr ^+ comma))
// exportStmt = 'export' optInd expr
//               ((comma expr)*
//               / 'except' optInd (expr ^+ comma))
// includeStmt = 'include' optInd expr ^+ comma
// fromStmt = 'from' expr 'import' optInd expr (comma expr)*
// returnStmt = 'return' optInd expr?
// raiseStmt = 'raise' optInd expr?
// yieldStmt = 'yield' optInd expr?
// discardStmt = 'discard' optInd expr?
// breakStmt = 'break' optInd expr?
// continueStmt = 'break' optInd expr?
// condStmt = expr colcom stmt COMMENT?
//            (IND{=} 'elif' expr colcom stmt)*
//            (IND{=} 'else' colcom stmt)?
// ifStmt = 'if' condStmt
// whenStmt = 'when' condStmt
// condExpr = expr colcom expr optInd
//         ('elif' expr colcom expr optInd)*
//          'else' colcom expr
// ifExpr = 'if' condExpr
// whenExpr = 'when' condExpr
// whileStmt = 'while' expr colcom stmt
// ofBranch = 'of' exprList colcom stmt
// ofBranches = ofBranch (IND{=} ofBranch)*
//                       (IND{=} 'elif' expr colcom stmt)*
//                       (IND{=} 'else' colcom stmt)?
// caseStmt = 'case' expr ':'? COMMENT?
//             (IND{>} ofBranches DED
//             | IND{=} ofBranches)
// tryStmt = 'try' colcom stmt &(IND{=}? 'except'|'finally')
//            (IND{=}? 'except' exprList colcom stmt)*
//            (IND{=}? 'finally' colcom stmt)?
// tryExpr = 'try' colcom stmt &(optInd 'except'|'finally')
//            (optInd 'except' exprList colcom stmt)*
//            (optInd 'finally' colcom stmt)?
// exceptBlock = 'except' colcom stmt
// blockStmt = 'block' symbol? colcom stmt
// blockExpr = 'block' symbol? colcom stmt
// staticStmt = 'static' colcom stmt
// deferStmt = 'defer' colcom stmt
// asmStmt = 'asm' pragma? (STR_LIT | RSTR_LIT | TRIPLESTR_LIT)
// genericParam = symbol (comma symbol)* (colon expr)? ('=' optInd expr)?
// genericParamList = '[' optInd
//   genericParam ^* (comma/semicolon) optPar ']'
// pattern = '{' stmt '}'
// indAndComment = (IND{>} COMMENT)? | COMMENT?
// routine = optInd identVis pattern? genericParamList?
//   paramListColon pragma? ('=' COMMENT? stmt)? indAndComment
// commentStmt = COMMENT
// section(RULE) = COMMENT? RULE / (IND{>} (RULE / COMMENT)^+IND{=} DED)
// enum = 'enum' optInd (symbol pragma? optInd ('=' optInd expr COMMENT?)? comma?)+
// objectWhen = 'when' expr colcom objectPart COMMENT?
//             ('elif' expr colcom objectPart COMMENT?)*
//             ('else' colcom objectPart COMMENT?)?
// objectBranch = 'of' exprList colcom objectPart
// objectBranches = objectBranch (IND{=} objectBranch)*
//                       (IND{=} 'elif' expr colcom objectPart)*
//                       (IND{=} 'else' colcom objectPart)?
// objectCase = 'case' identWithPragma ':' typeDesc ':'? COMMENT?
//             (IND{>} objectBranches DED
//             | IND{=} objectBranches)
// objectPart = IND{>} objectPart^+IND{=} DED
//            / objectWhen / objectCase / 'nil' / 'discard' / declColonEquals
// object = 'object' pragma? ('of' typeDesc)? COMMENT? objectPart
// typeClassParam = ('var' | 'out')? symbol
// typeClass = typeClassParam ^* ',' (pragma)? ('of' typeDesc ^* ',')?
//               &IND{>} stmt
// typeDef = identWithPragmaDot genericParamList? '=' optInd typeDefAux
//             indAndComment? / identVisDot genericParamList? pragma '=' optInd typeDefAux
//             indAndComment?
// varTuple = '(' optInd identWithPragma ^+ comma optPar ')' '=' optInd expr
// colonBody = colcom stmt postExprBlocks?
// variable = (varTuple / identColonEquals) colonBody? indAndComment
// constant = (varTuple / identWithPragma) (colon typeDesc)? '=' optInd expr indAndComment
// bindStmt = 'bind' optInd qualifiedIdent ^+ comma
// mixinStmt = 'mixin' optInd qualifiedIdent ^+ comma
// pragmaStmt = pragma (':' COMMENT? stmt)?
// simpleStmt = ((returnStmt | raiseStmt | yieldStmt | discardStmt | breakStmt
//            | continueStmt | pragmaStmt | importStmt | exportStmt | fromStmt
//            | includeStmt | commentStmt) / exprStmt) COMMENT?
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
// stmt = (IND{>} complexOrSimpleStmt^+(IND{=} / ';') DED)
//      / simpleStmt ^+ ';'

    }

}
