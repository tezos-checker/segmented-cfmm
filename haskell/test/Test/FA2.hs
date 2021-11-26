-- SPDX-FileCopyrightText: 2021 Arthur Breitman
-- SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

module Test.FA2
  ( test_FA2_positions
  ) where

import Universum

import qualified Data.Set as Set
import Hedgehog hiding (assert, failure)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Morley.Nettest
import Test.Tasty (TestTree)
import Util.Named

import Test.SegCFMM.Contract
import Test.Util

-- | Utility type used for the generation of the test below
data OwnershipType
  = FullyOwned
  | Operated
  | OwnedOnly
  | FinalOwner
  deriving stock (Eq, Enum, Bounded, Ord, Show)

{-# ANN test_FA2_positions ("HLint: ignore Avoid lambda using `infix`" :: Text) #-}
-- | Generic property tests for FA2-compliant position handling.
test_FA2_positions :: TestTree
test_FA2_positions =
  propOnNetwork "FA2 position are transferred correctly"
    do
      -- generate up to 20 unique positions (owner type + indexes)
      positionIndexes <- fmap Set.toList . forAll . Gen.set (Range.linear 0 20) $ do
        lowerTickIndex <- Gen.integral (Range.linearFrom 0 -10000 9990)
        upperTickIndex <- Gen.integral (Range.linear (lowerTickIndex + 1) (lowerTickIndex + 10))
        ownerType <- Gen.enumBounded
        return (ownerType, lowerTickIndex, upperTickIndex)

      -- randomly shuffle them, to interleave position ids
      forAll $ Gen.shuffle positionIndexes
    [ (FullyOwned, -100, 100)
    , (Operated, -100, 100)
    , (OwnedOnly, 0, 100)
    , (FinalOwner, 100, 200)
    ]
    \sortedPositions -> do
      -- may own some positions and operate others from the next owner
      ownerAndOperator <- newAddress auto
      -- may own some positions, some of which operated by the 'ownerAndOperator'
      ownerOnly <- newAddress auto
      -- may own some position and eventually will own all of them
      finalOwner <- newAddress auto
      let accounts = [ownerAndOperator, ownerOnly, finalOwner]
      cfmm <- fst <$> prepareSomeSegCFMM accounts defaultTokenTypes def
      inBatch $ for_ accounts \acc -> transferMoney acc 10_e6

      -- create positions
      let ownerFor ownerType = case ownerType of
            FullyOwned -> ownerAndOperator
            Operated -> ownerOnly
            OwnedOnly -> ownerOnly
            FinalOwner -> finalOwner

      let createPosition (n, posLst) (ownerType, lowerTickIndex, upperTickIndex) = do
            withSender (ownerFor ownerType) $ setPosition cfmm 1_e7 (lowerTickIndex, upperTickIndex)
            return (n + 1, (FA2.TokenId n, ownerType) : posLst)

      positionsList <- snd <$> foldM createPosition (0, []) sortedPositions

      -- divided lists
      let selectByType ownerType = map fst $ filter ((== ownerType) . snd) positionsList
          ownerPositions = selectByType FullyOwned
          operatedPositions = selectByType Operated
          ownedOnlyPositions = selectByType OwnedOnly
          finalOwnerPositions = selectByType FinalOwner
          allPositions = map fst positionsList

      -- assign operators
      withSender ownerOnly $ inBatch do
        for_ operatedPositions $ \positionId ->
          updateOperator cfmm ownerOnly ownerAndOperator positionId True

      -- the 'ownerAndOperator' can transfer its own positions
      withSender ownerAndOperator $ inBatch do
        for_ ownerPositions $ \positionId ->
          transferToken' cfmm ownerAndOperator finalOwner positionId

      -- the 'ownerAndOperator' cannot transfer positions it's not operator of
      withSender ownerAndOperator $
        for_ ownedOnlyPositions $ \positionId ->
          expectCustomError_ #fA2_NOT_OPERATOR $
            transferToken' cfmm ownerOnly finalOwner positionId

      withSender ownerAndOperator $
        for_ finalOwnerPositions $ \positionId ->
          expectCustomError_ #fA2_NOT_OPERATOR $
            transferToken' cfmm finalOwner ownerOnly positionId

      -- the 'ownerAndOperator' can transfer positions it's operator of
      withSender ownerAndOperator $ inBatch do
        for_ operatedPositions $ \positionId ->
          transferToken' cfmm ownerOnly finalOwner positionId

      -- the 'ownerOnly' can no longer transfer the operated positions
      withSender ownerOnly $
        for_ operatedPositions $ \positionId ->
          expectCustomError #fA2_INSUFFICIENT_BALANCE (#required .! 1, #present .! 0) $
            transferToken' cfmm ownerOnly finalOwner positionId

      -- the 'ownerOnly' can still transfer the owned positions
      withSender ownerOnly $ inBatch do
        for_ ownedOnlyPositions $ \positionId ->
          transferToken' cfmm ownerOnly finalOwner positionId

      -- in the end all positions should be owned by 'finalOwner'
      for_ allPositions $ \positionId -> do
        balanceOf (TokenInfo positionId cfmm) ownerAndOperator @@== 0
        balanceOf (TokenInfo positionId cfmm) ownerOnly        @@== 0
        balanceOf (TokenInfo positionId cfmm) finalOwner       @@== 1
