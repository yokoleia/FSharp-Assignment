module CAB402.FSharp.GeneticAlgorithm

open RandomMonad

// The genes of one individual within the population. Each gene is an integer.
type Individual = int array

// The genes of an individual together with an assessment of the fitness of that individual
type ScoredIndividual = Individual * float

// a collection of scored individuals that make up a generation
type Population = ScoredIndividual array

// custom function to print genes as a string to the console
let printgenes (name: string) (individual: Individual) =
    System.Console.Write("{0}: ", name)

    for gene in individual do
        System.Console.Write("{0}", gene)

    System.Console.Write("\n")


// Find an individual within the population that has the highest fitness
let fitest (population: Population) : ScoredIndividual = population |> Array.maxBy snd
// DONE




// Given a set of competeting individuals, return the winning individual (i.e. one with best fitness)
let tournamentWinner (competitors: Population) : Individual =
    // TODO: add correct implementation here
    // raise (System.NotImplementedException "tournamentWinner")
    let scoredtournamentWinner = fitest competitors
    let tournamentWinner = fst scoredtournamentWinner
    tournamentWinner

// DONE

// Randomly select an individual from a population by conducting a tournament.
// A field of n = 2 competitors is first randomly generated and then the best individual within tht field is selected.
// Each of individuals selected for the competition is selected independently,
// so it is possible that the same individual may be selected more than once.
let tournamentSelect (population: Population) : Rand<Individual> =
    // TODO: add correct implementation here
    // raise (System.NotImplementedException "tournamentSelect")

    let populationLength = population.Length

    rand {
        let! random1 = intRange 0 populationLength
        let! random2 = intRange 0 populationLength

        let tournamentFinal : Population =
            [| population.[random1]
               population.[random2] |]

        let Winner = tournamentWinner (tournamentFinal)

        return Winner

    }
// DONE

// Combine the genes of parent1 and parent2 based on the given splitPoint
// The splitpoint will always be between 1 and length-1, where length is the length of both parent genes)
// The genes in position 0..splitPoint will come directly from the corresponding genes of parent1
// It is important the genes of the generated child is a legal permutation
// i.e. it should include each of the integers between 0 and length-1 precisely once.
// The order of the remaining genes of the child (those that were not inherited from parent1) are
// determined by the order that they occurred in parent2.
// For example if parent1 = [0,3,5,4,2,1,6] and parent2 = [6,4,2,1,0,3,5] and the splitpoint is 4
// then the first 4 genes come from parent1 [0,3,5,4] and the remaining genes [2,1,6] are ordered
// according to parent2 i.e. [6,2,1] because 6 comes before 2 and 2 comes before 1 in parent2.
// So the child in this example will be [0,3,5,4,6,2,1]
let crossAt (parent1: Individual) (parent2: Individual) (splitPoint: int) : Individual =
    // TODO: add correct implementation here
    // raise (System.NotImplementedException "crossAt")

    let left, right = parent1 |> Array.splitAt splitPoint

    let sortedRightSeq =
        seq {
            for gene in parent2 do
                if right |> Array.contains gene then
                    yield gene
        }

    let sortedRight = sortedRightSeq |> Seq.toArray

    let child = sortedRight |> Array.append left

    // printgenes "child" child

    child

// DONE



// Combine the genes of parent1 and parent2 at a randonly choosen splitpoint as per the above crossAt algorithm
// The splitpoint is chosen so that both parents provide at least one gene to the child
let cross (parent1: Individual) (parent2: Individual) : Rand<Individual> =
    // TODO: add correct implementation here
    // raise (System.NotImplementedException "cross")


    rand {
        let! random1 = intRange 1 (parent1.Length - 1)

        let child : Individual = crossAt parent1 parent2 random1



        return child

    }

// DONE


// Return a mutated version of the original genes
// the sequence of genes is split into 3 sections, a start, middle and end, based on the 2 provided indexes
// (where 0 <= firstIndex < secondIndex < genes.length)
// The start and end sections of the genes are left intact, while the genes in the middle section are reversed in order.
// For example reverseMutateAt [0,3,5,4,2,1,6] 2 4 = [0,3,2,4,5,1,6]
let reverseMutateAt (genes: Individual) (firstIndex: int) (secondIndex: int) : Individual =
    // TODO: add correct implementation here
    // raise (System.NotImplementedException "reverseMutateAt")

    let left, leftoverRight = genes |> Array.splitAt firstIndex

    let middle, right =
        leftoverRight
        |> Array.splitAt (secondIndex - left.Length + 1)

    let mutation = middle |> Array.rev

    let halfChild = mutation |> Array.append left
    let finalGenes = right |> Array.append halfChild


    finalGenes

// DONE


// Perform a reverse mutation based on two randomly chosen split points
// (such that 0 <= firstIndex < secondIndex < genes.length)
let reverseMutate (chromosome: Individual) : Rand<Individual> =
    // TODO: add correct implementation here
    // raise (System.NotImplementedException "reverseMutate")
    printgenes "reverseMutate:" chromosome

    rand {
        let! firstIndex = intRange 0 (chromosome.Length - 1)
        let! secondIndex = intRange (firstIndex + 1) (chromosome.Length)


        let child : Individual =
            reverseMutateAt chromosome firstIndex secondIndex

        System.Console.WriteLine("reverseMutate: firstIndex: {0}, SecondIndex {1}", firstIndex, secondIndex)

        return child
    }

// DONE


let MutateProbability = 0.15




// Perform a reverse mutation of the given genes with probability 0.15,
// otherwise leave the sequence unaltered.
let possiblyMutate (genes: Individual) : Rand<Individual> =

    rand {
        let! odds = withProbability MutateProbability
        let! possiblyMutatedGenes = reverseMutate genes

        if odds then
            return possiblyMutatedGenes
        else
            return genes
    }

// DONE


// Create a new population that consists of all of the children, plus the 10 best individuals from the previous generation.
let elitismSelection (parents: Population) (children: Population) : Population =
    // TODO: add correct implementation here

    let sortedParents = Array.sortBy (fun (_, y) -> -y) parents

    let topParents, leftovers = sortedParents |> Array.splitAt 10
    let newPopulation = topParents |> Array.append children
    newPopulation


// Create a scored individual by applying the fitness function to assess the fitness of the given genes.
let score (fitnessFunction: Individual -> float) (genes: Individual) : ScoredIndividual =
    // TODO: add correct implementation here

    let score : ScoredIndividual = (genes, (fitnessFunction genes))
    score
// DONE






// Randomly generate a population containing the specified number of individuals, each with the specified number of genes.
let randomIndividuals
    (fitnessFunction: Individual -> float)
    (numberGenes: int)
    (numberIndividuals: int)
    : Rand<Population> =
    // TODO: add correct implementation here
    raise (System.NotImplementedException "randomIndividuals")
    //let population = randArrayUnfold
    (*
    rand {

        let individuals = Seq.init numberIndividuals (fun _ !-> 
            randomPermutation numberGenes)

        let scoredIndividuals =
            individuals
            |> Seq.map (fun individual -> score fitnessFunction individual)
        // what is this doing here?
// fake return val so it compiles

        let! ind = randomPermutation numberGenes
        let scoredInd = score fitnessFunction ind




        printfn "Scored Individuals %A" individuals

        let population = [| scoredInd |]
        return population
    }


    rand {
        ind = 
    }


// randompermutation generates a wrapped Individual
// therefore bang


// create scored ind generator
// create ind
// score ind



//let population = [|scoredInd,scoredInd,scoredInd,scoredInd,scoredInd,scoredInd,scoredInd,scoredInd,scoredInd,scoredInd|]

// return population




(*
        Severity	Code	Description	Project	File	Line	Suppression State
        Error	FS0001	The 'if' expression needs to have type '('a * int) option' to satisfy context type requirements. It currently has type 'Rand<int array>'.
        FSharpGeneticAlgorithm	E:\uni\IN02 - Bachelor of Information Technology\2021\Semester 1\Programming Paradigms\CAB402AssignmentOneSkeletonSolution\GeneticAlgorithm\Program.fs	250	Active
        *)

(*
            randArrayUnfold pickNext collection




        let students =
           Map.empty. (* Creating an empty Map *)
              let ind = randomPermutation numberGenes
              Add(ind).
              ;;

        let ind:Individual = randomPermutation numberGenes
        // [1, 2, 3, 4, 5 ,6, 7,8 ,9 ,10] - int[]
        // [1, 2, 3, 4, 5 ,6, 7,8 ,9 ,10] - Rand<int[]>
        let scoredInd = scored fitnessFunction ind

        *)


//val individuals: List<Individuals> = generateIndividuals(x)
//val scoredIndividuals: List<ScoredIndividuals> = individuals.map{ individual: Individual -> score(invidual)}
//val population = Population(scoredIndividuals)





// now i need to generate a array of numberIndividuals length by appending a new scoredInd value

// either using randArrayUnfold: custom method to unfold but needs above code in a generator
// (which can't do because bang

// or have this in a for loop inside a sequence generator maybe? idk

// population is an array of type ScoredIndividual



//let! population = randArrayUnfold generator scoredInd
// Person(name, (*  *)int)

// generate an individual (array ints)
// generate scoredind (indiv, score)






//return population
//}



//map{ x: Rand<Individual> -> x.toArray()}


//

//let (ind:Rand<Individual>) = randomPermutation numberGenes

// HOW DO I UNWRAP THE RAND

// let scoredind = score fitnessFunction ind



//let state1 = generator
//let population = randArrayUnfold randomPermutation state1
//population

//    printgenes "gene 1" ind





//let population = [|1..numberIndividuals|] |> List.map |> score fitnessFunction ind


(*

        let myseq = seq {
            for i in 1 .. 10 do
                let (ind:Rand<Individual>) =
                yield! ind
        }


        The use of 'let! x = coll' in sequence expressions is not permitted.
        Use 'for x in coll' instead.




        let population = myseq |> Seq.toArray
        *)
//return population



(*
        let population = seq |> Seq.toArray //[|scoredInd;|]
        return population
        *)


//}
//randArrayInit
//randArrayUnfold
(*let basicgenestructure = [| for i in 0 .. (numberGenes-1) -> i |]
    let genes:Rand<Individual> = shuffle basicgenestructure

    System.Console.WriteLine("{0}", genes.ToString)

    let generate genestructure =


    rand {





    }
    *)






// generate an individual as an int array

// score individual
// append scored individual to array
// return population i.e. scoredindividual array



*)





// Generate a child by first randomly choosing two parents using tournament selection,
// cross their genes and then optionally mutate the resulting genes.
// Note: individuals have no gender and each parent is chosen independently,
// so there is a small chance that the same individual may be choosen twice.
let procreate fitnessFunction (population: Population) : Rand<ScoredIndividual> =
    // TODO: add correct implementation here
    // raise (System.NotImplementedException "procreate")

    let finalGenes =

        rand {
            let! parent1 = tournamentSelect population
            let! parent2 = tournamentSelect population

            let! child = cross parent1 parent2
            let! genes = possiblyMutate child


            // return rand<Individual>


            let finalGenes = score fitnessFunction genes

            return finalGenes

        }

    finalGenes


// DONE

// Create a new generation by creating the specified number of children through procreation and then
// applying elitism selection to create the population of the next generation
let evolveOneGeneration fitnessFunction (parentPopulation: Population) (childPopulationLimit: int) : Rand<Population> =
    // TODO: add correct implementation here
    raise (System.NotImplementedException "evolveOneGeneration")




// Starting with the specified initial population, evolve generation by generation (forever).
// For each population, we determine the fitest individual from that generation and return an infinite sequence of these individuals.
// Due to elitism selection, the fitest individual in each succcessive generation should be at least as good as the previous generation.
let evolveForever
    fitnessFunction
    (initialPopulation: Population)
    (childPopulationLimit: int)
    : Rand<ScoredIndividual seq> =
    // TODO: add correct implementation here
    raise (System.NotImplementedException "evolveForever")

let Optimize fitnessFunction numberOfGenes numerOfIndividuals : ScoredIndividual seq =
    let solutions =
        rand {
            let! initialPopulation = randomIndividuals fitnessFunction numberOfGenes numerOfIndividuals
            return! evolveForever fitnessFunction initialPopulation numerOfIndividuals
        }

    let random = new System.Random()
    RandomMonad.evaluateWith random solutions
