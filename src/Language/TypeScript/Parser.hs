-----------------------------------------------------------------------------
--
-- Module      :  Language.TypeScript.Parser
-- Copyright   :  (c) DICOM Grid Inc. 2013
-- License     :  MIT
--
-- Maintainer  :  Phillip Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Language.TypeScript.Parser (
    declarationSourceFile,
    nextIdentifier
) where

import Language.TypeScript.Types
import Language.TypeScript.Lexer

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.String (parseFromFile)
import Control.Applicative
       (Applicative(..), (<$>), (<*>), (<*), (*>))

commentPlaceholder = fmap toOffset getPosition where
  toOffset pos = Left $ (sourceLine pos, sourceColumn pos)

nextIdentifier =
    skipMany (choice  (map (try . reserved) [ "export", "declare", "public", "private", "static" ]))
    >> choice (map (try . reserved) [ "var", "function", "class", "interface", "enum", "module" ])
    >> identifier

declarationSourceFile = whiteSpace >> many declarationElement <* eof

exported = reserved "export" >> return Exported

declarationElement = choice $ map try
  [ InterfaceDeclaration <$> commentPlaceholder <*> optionMaybe exported <*> interface
  , ExportDeclaration <$> (reserved "export" >> lexeme (char '=') *> identifier)
  , ExternalImportDeclaration <$> optionMaybe exported <*> (reserved "import" *> identifier) <*> (lexeme (char '=') *> reserved "require" *> parens stringLiteral <* semi)
  , ImportDeclaration <$> optionMaybe exported <*> (reserved "import" *> identifier) <*> (lexeme (char '=') *> entityName)
  , AmbientDeclaration <$> commentPlaceholder <*> optionMaybe exported <*> (reserved "declare" *> ambientDeclaration)
  ]

ambientDeclaration = choice (map try
  [ ambientVariableDeclaration
  , ambientFunctionDeclaration
  , ambientClassDeclaration
  , ambientInterfaceDeclaration
  , ambientEnumDeclaration
  , ambientModuleDeclaration
  , ambientExternalModuleDeclaration
  ])

ambientVariableDeclaration = AmbientVariableDeclaration <$> commentPlaceholder <*> (reserved "var" *> identifier) <*> (optionMaybe typeAnnotation <* semi)

ambientFunctionDeclaration = AmbientFunctionDeclaration <$> commentPlaceholder <*> (reserved "function" *> identifier) <*> (parameterListAndReturnType <* semi)

ambientClassDeclaration = AmbientClassDeclaration <$> commentPlaceholder <*> (reserved "class" *> identifier) <*> optionMaybe typeParameters <*> optionMaybe extendsClause <*> optionMaybe implementsClause <*> braces (sepEndBy ambientClassBodyElement semi)

ambientInterfaceDeclaration = AmbientInterfaceDeclaration <$> interface

ambientEnumDeclaration = AmbientEnumDeclaration <$> commentPlaceholder <*> (reserved "enum" *> identifier) <*> braces (sepEndBy enumMember comma)
  where
  enumMember = (,) <$> propertyName <*> optionMaybe (lexeme (char '=') >> integer)

ambientModuleDeclaration = AmbientModuleDeclaration <$> commentPlaceholder <*> (reserved "module" *> sepBy identifier dot) <*> braces (many ambientDeclaration)

ambientExternalModuleDeclaration = AmbientExternalModuleDeclaration <$> commentPlaceholder <*> (reserved "module" *> stringLiteral) <*> braces (many ambientExternalModuleElement)

ambientExternalModuleElement = choice (map try
  [ AmbientModuleElement <$> ambientDeclaration
  , exportAssignment
  , externalImportDeclaration ])

exportAssignment = ExportAssignment <$> (reserved "export" *> lexeme (char '=') *> identifier <* semi)

externalImportDeclaration =
  AmbientModuleExternalImportDeclaration <$> optionMaybe exported
                                         <*> (reserved "import" *> identifier)
                                         <*> (lexeme (char '=') *> reserved "require" *> stringLiteral)

ambientClassBodyElement = (,) <$> commentPlaceholder <*> (choice $ map try
  [ ambientConstructorDeclaration
  , ambientMemberDeclaration
  , ambientIndexSignature ])

ambientConstructorDeclaration = AmbientConstructorDeclaration <$> (reserved "constructor" *> parameterList <* semi)

ambientMemberDeclaration = AmbientMemberDeclaration <$> optionMaybe publicOrPrivate <*> optionMaybe static <*> propertyName <*> choice [fmap Right parameterListAndReturnType, fmap Left (optionMaybe typeAnnotation)]

ambientIndexSignature = AmbientIndexSignature <$> indexSignature

interface = Interface <$> commentPlaceholder <*> (reserved "interface" *> identifier) <*> optionMaybe typeParameters <*> optionMaybe extendsClause <*> objectType

extendsClause = reserved "extends" >> classOrInterfaceTypeList

implementsClause = reserved "implements" >> classOrInterfaceTypeList

classOrInterfaceTypeList = commaSep typeRef

objectType = braces typeBody

typeBody = TypeBody <$> sepEndBy typeMember semi
  where
  typeMember = (,) <$> commentPlaceholder <*> (choice $ map try [ methodSignature, propertySignature, callSignature, constructSignature, typeIndexSignature ])

propertySignature = PropertySignature <$> propertyName <*> optionMaybe (lexeme (char '?' >> return Optional)) <*> optionMaybe typeAnnotation

propertyName = identifier <|> stringLiteral

typeAnnotation = colon >> _type

callSignature = CallSignature <$> parameterListAndReturnType

parameterListAndReturnType = ParameterListAndReturnType <$> optionMaybe typeParameters <*> parens parameterList <*> optionMaybe typeAnnotation

parameterList = commaSep parameter

parameter = choice
  [ try $ RequiredOrOptionalParameter <$> optionMaybe publicOrPrivate <*> identifier <*> optionMaybe (lexeme (char '?' >> return Optional)) <*> optionMaybe typeAnnotation
  , RestParameter <$> (lexeme (string "...") *> identifier) <*> optionMaybe typeAnnotation
  ]

static = reserved "static" >> return Static

publicOrPrivate = choice
  [ reserved "public" >> return Public
  , reserved "private" >> return Private ]

stringOrNumber = choice
  [ reserved "string" >> return String
  , reserved "number" >> return Number ]

constructSignature = ConstructSignature <$> (reserved "new" *> optionMaybe typeParameters) <*> parens parameterList <*> optionMaybe typeAnnotation

typeIndexSignature = TypeIndexSignature <$> indexSignature

indexSignature = squares (IndexSignature <$> identifier <*> (colon *> stringOrNumber)) <*> typeAnnotation

methodSignature = MethodSignature <$> propertyName <*> optionMaybe (lexeme (char '?' >> return Optional)) <*> parameterListAndReturnType

typeParameters = angles $ commaSep1 typeParameter

typeParameter = TypeParameter <$> identifier <*> optionMaybe (reserved "extends" >> _type)
fold :: Stream s m t => ParsecT s u m a -> ParsecT s u m b -> (a -> b -> a) -> ParsecT s u m a
fold first more combine = do
  a <- first
  bs <- many more
  return $ foldl combine a bs

_type = lexeme $ choice [ arrayType, functionType, constructorType ]
  where
  arrayType = fold atomicType (squares whiteSpace) (flip $ const ArrayType)
  atomicType = choice $ map try
    [ Predefined <$> predefinedType
    , TypeReference <$> typeRef
    , ObjectType <$> objectType
    ]
  functionType = FunctionType <$> optionMaybe typeParameters <*> parens parameterList <*> returnType
  constructorType = ConstructorType <$> (reserved "new" *> optionMaybe typeParameters) <*> parens parameterList <*> returnType
  returnType = lexeme (string "=>") *> _type

typeRef = TypeRef <$> typeName <*> optionMaybe typeArguments

predefinedType = choice
  [ reserved "any" >> return AnyType
  , reserved "number" >> return NumberType
  , (reserved "boolean" <|> reserved "bool") >> return BooleanType
  , reserved "string" >> return StringType
  , reserved "void" >> return VoidType
  ]

entityName = fmap toEntityName (sepBy1 identifier dot)
  where
  toEntityName [t] = EntityName Nothing t
  toEntityName ts = EntityName (Just $ ModuleName $ init ts) (last ts)

typeName = fmap toTypeName (sepBy1 identifier dot)
  where
  toTypeName [t] = TypeName Nothing t
  toTypeName ts = TypeName (Just $ ModuleName $ init ts) (last ts)

typeArguments = angles $ commaSep1 _type
