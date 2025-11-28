{-# LANGUAGE DataKinds #-}

{- |
Module      : AdultDataset
Description : Adult Census Dataset types, parsing, and loading

This module contains all the types and functionality for working with
the Adult Census Dataset, including sum types for categorical data,
parsing functions, and dataset loading utilities.
-}
module AdultDataset (
    -- * Data types
    Adult (..),
    WorkClass (..),
    Education (..),
    MaritalStatus (..),
    Occupation (..),
    Relationship (..),
    Race (..),
    Sex (..),
    Income (..),

    -- * Dataset loading
    loadAdultDataset,
) where

import Core (Dataset (Dataset))
import Data.List.Split (splitOn)
import Text.Read (readMaybe)

-- ============================================================================
-- Adult Census Dataset Sum Types
-- ============================================================================

-- | Work class categories
data WorkClass
    = Private
    | SelfEmpNotInc
    | SelfEmpInc
    | FederalGov
    | LocalGov
    | StateGov
    | WithoutPay
    | NeverWorked
    | UnknownWorkClass
    deriving (Show, Eq, Ord)

-- | Education levels
data Education
    = Bachelors
    | SomeCollege
    | EleventhGrade
    | HSGrad
    | ProfSchool
    | AssocAcdm
    | AssocVoc
    | NinthGrade
    | SeventhEighthGrade
    | TwelfthGrade
    | Masters
    | FirstFourthGrade
    | TenthGrade
    | Doctorate
    | FifthSixthGrade
    | Preschool
    deriving (Show, Eq, Ord)

-- | Marital status
data MaritalStatus
    = MarriedCivSpouse
    | Divorced
    | NeverMarried
    | Separated
    | Widowed
    | MarriedSpouseAbsent
    | MarriedAFSpouse
    deriving (Show, Eq, Ord)

-- | Occupation types
data Occupation
    = TechSupport
    | CraftRepair
    | OtherService
    | Sales
    | ExecManagerial
    | ProfSpecialty
    | HandlersCleaners
    | MachineOpInspct
    | AdmClerical
    | FarmingFishing
    | TransportMoving
    | PrivHouseServ
    | ProtectiveServ
    | ArmedForces
    | UnknownOccupation
    deriving (Show, Eq, Ord)

-- | Relationship types
data Relationship
    = Wife
    | OwnChild
    | Husband
    | NotInFamily
    | OtherRelative
    | Unmarried
    deriving (Show, Eq, Ord)

-- | Race categories
data Race
    = White
    | AsianPacIslander
    | AmerIndianEskimo
    | Other
    | Black
    deriving (Show, Eq, Ord)

-- | Sex
data Sex = Male | Female
    deriving (Show, Eq, Ord)

-- | Income bracket
data Income = LowIncome | HighIncome
    deriving (Show, Eq, Ord)

-- | Adult census record structure with proper sum types
data Adult = Adult
    { age :: Int
    , workclass :: WorkClass
    , fnlwgt :: Int
    , education :: Education
    , educationNum :: Int
    , maritalStatus :: MaritalStatus
    , occupation :: Occupation
    , relationship :: Relationship
    , race :: Race
    , sex :: Sex
    , capitalGain :: Int
    , capitalLoss :: Int
    , hoursPerWeek :: Int
    , nativeCountry :: String  -- Keep as String due to many country codes
    , income :: Income
    }
    deriving (Show, Eq)

-- ============================================================================
-- Parsing Functions
-- ============================================================================

-- | Parse work class from string
parseWorkClass :: String -> WorkClass
parseWorkClass s = case trim s of
    "Private" -> Private
    "Self-emp-not-inc" -> SelfEmpNotInc
    "Self-emp-inc" -> SelfEmpInc
    "Federal-gov" -> FederalGov
    "Local-gov" -> LocalGov
    "State-gov" -> StateGov
    "Without-pay" -> WithoutPay
    "Never-worked" -> NeverWorked
    _ -> UnknownWorkClass
  where
    trim = filter (/= ' ')

-- | Parse education from string
parseEducation :: String -> Education
parseEducation s = case trim s of
    "Bachelors" -> Bachelors
    "Some-college" -> SomeCollege
    "11th" -> EleventhGrade
    "HS-grad" -> HSGrad
    "Prof-school" -> ProfSchool
    "Assoc-acdm" -> AssocAcdm
    "Assoc-voc" -> AssocVoc
    "9th" -> NinthGrade
    "7th-8th" -> SeventhEighthGrade
    "12th" -> TwelfthGrade
    "Masters" -> Masters
    "1st-4th" -> FirstFourthGrade
    "10th" -> TenthGrade
    "Doctorate" -> Doctorate
    "5th-6th" -> FifthSixthGrade
    "Preschool" -> Preschool
    _ -> HSGrad  -- Default fallback
  where
    trim = filter (/= ' ')

-- | Parse marital status from string
parseMaritalStatus :: String -> MaritalStatus
parseMaritalStatus s = case trim s of
    "Married-civ-spouse" -> MarriedCivSpouse
    "Divorced" -> Divorced
    "Never-married" -> NeverMarried
    "Separated" -> Separated
    "Widowed" -> Widowed
    "Married-spouse-absent" -> MarriedSpouseAbsent
    "Married-AF-spouse" -> MarriedAFSpouse
    _ -> NeverMarried  -- Default fallback
  where
    trim = filter (/= ' ')

-- | Parse occupation from string
parseOccupation :: String -> Occupation
parseOccupation s = case trim s of
    "Tech-support" -> TechSupport
    "Craft-repair" -> CraftRepair
    "Other-service" -> OtherService
    "Sales" -> Sales
    "Exec-managerial" -> ExecManagerial
    "Prof-specialty" -> ProfSpecialty
    "Handlers-cleaners" -> HandlersCleaners
    "Machine-op-inspct" -> MachineOpInspct
    "Adm-clerical" -> AdmClerical
    "Farming-fishing" -> FarmingFishing
    "Transport-moving" -> TransportMoving
    "Priv-house-serv" -> PrivHouseServ
    "Protective-serv" -> ProtectiveServ
    "Armed-Forces" -> ArmedForces
    _ -> UnknownOccupation
  where
    trim = filter (/= ' ')

-- | Parse relationship from string
parseRelationship :: String -> Relationship
parseRelationship s = case trim s of
    "Wife" -> Wife
    "Own-child" -> OwnChild
    "Husband" -> Husband
    "Not-in-family" -> NotInFamily
    "Other-relative" -> OtherRelative
    "Unmarried" -> Unmarried
    _ -> NotInFamily  -- Default fallback
  where
    trim = filter (/= ' ')

-- | Parse race from string
parseRace :: String -> Race
parseRace s = case trim s of
    "White" -> White
    "Asian-Pac-Islander" -> AsianPacIslander
    "Amer-Indian-Eskimo" -> AmerIndianEskimo
    "Other" -> Other
    "Black" -> Black
    _ -> Other  -- Default fallback
  where
    trim = filter (/= ' ')

-- | Parse sex from string
parseSex :: String -> Sex
parseSex s = case trim s of
    "Male" -> Male
    "Female" -> Female
    _ -> Male  -- Default fallback
  where
    trim = filter (/= ' ')

-- | Parse income from string
parseIncome :: String -> Income
parseIncome s = case trim s of
    ">50K" -> HighIncome
    "<=50K" -> LowIncome
    _ -> LowIncome  -- Default fallback
  where
    trim = filter (/= ' ')

-- | Parse a CSV line into an Adult record
parseAdult :: String -> Maybe Adult
parseAdult line =
    case splitOn "," line of
        [ageStr, wc, fnlStr, edu, eduNumStr, ms, occ, rel, r, s, cgStr, clStr, hpwStr, nc, inc] ->
            Adult
                <$> readMaybe ageStr
                <*> pure (parseWorkClass wc)
                <*> readMaybe fnlStr
                <*> pure (parseEducation edu)
                <*> readMaybe eduNumStr
                <*> pure (parseMaritalStatus ms)
                <*> pure (parseOccupation occ)
                <*> pure (parseRelationship rel)
                <*> pure (parseRace r)
                <*> pure (parseSex s)
                <*> readMaybe cgStr
                <*> readMaybe clStr
                <*> readMaybe hpwStr
                <*> pure (trim nc)
                <*> pure (parseIncome inc)
        _ -> Nothing
  where
    trim = filter (/= ' ')

-- ============================================================================
-- Dataset Loading
-- ============================================================================

-- | Load adult dataset from CSV file
loadAdultDataset :: IO (Dataset 1 Adult)
loadAdultDataset = do
    content <- readFile "adult.csv"
    let csvLines = drop 1 $ lines content  -- Skip header
        adults = [adult | line <- csvLines, Just adult <- [parseAdult line]]
    return $ Dataset adults