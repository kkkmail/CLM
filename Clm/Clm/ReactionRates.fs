namespace Clm

open System
open System.Collections.Generic
open FSharp.Collections
open System.Numerics
open MathNet.Numerics.LinearAlgebra
open Clm.Substances

module ReactionRates = 

    /// Abstraction over all known statistical distrubiton parameters.
    type DistributionParams =
        {
            seed : int option
        }

    /// Abstraction over statistical distributons.
    type Distribution (distributionParams : DistributionParams) =

        /// Gives next double from the distrubution.
        /// It is NOT [0, 1) based but, rather based on the parameters of the distribution.
        /// This is crucial for long-tailed distributions, which often do not have standard deviation and may not even have mean defined.
        member dstr.nextDouble() : double = 0.0

        /// Gives next bool from the distribution.
        member dstr.nextBool() : double = 0.0

    /// Models, which describe catalytic reactions.
    type CatalystModel = 
        | X // If substance is a catalyst for some amino acid X, then there are some [separate] chances that it could be a catalyst for other amino acids.

    //| ReactionRateProvider of (ReactionInfo -> (ReactionRate option * ReactionRate option))

    type ReactionRates () = 
        let x = 1

        member this.a = 0

