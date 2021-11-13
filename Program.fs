open System
open System.Collections.Generic
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running


type BufferState =
    | Full
    | Partial
    | Empty



// Parameters for generating test data
let rng = Random (123)
let maxLevelValue = 100.0
let maxRateValue = 100.0
// How many lookups we will perform in our test
let lookupCount = 10_000

// Maximum length of the arrays
let maxLevelsLength = 100
let maxRatesLength = 100
let maxBufferStateLength = 100

// How many arrays to generate
let levelsCount  = 1_000
let maxRatesCount = 1_000
let buffersCount = 1_000

// How many different random Settings to generate
let randomSettingsCount = 1_000


// Function to create an array<float> with random values
let randomFloatArray (rng: Random) maxValue length =
    let result = Array.zeroCreate length

    for i = 0 to result.Length - 1 do
        result.[i] <- maxValue * (rng.NextDouble ())

    result

// Function to create an array<BufferState> with random values
let randomBuffersArray (rng: Random) length =
    let result = Array.zeroCreate length

    for i = 0 to result.Length - 1 do
        // The `Next` method is generating values from 0 to 2
        // The MaxValue arg used here is exclusive
        match rng.Next 3 with
        | 0 -> result.[i] <- BufferState.Empty
        | 1 -> result.[i] <- BufferState.Full
        | 2 -> result.[i] <- BufferState.Partial
        | _ -> failwith "Really?"

    result

// Generate possible array<float> to be used as the Levels field
let levels =
    seq {
        for _ in 1 .. levelsCount ->
            ((rng.Next maxLevelsLength) + 1) // Generate a random length
            |> randomFloatArray rng maxLevelValue
    } |> Array.ofSeq

// Generate possible array<float> to be used as the MaxRates field
let maxRates =
    seq {
        for _ in 1 .. maxRatesCount ->
            ((rng.Next maxRatesLength) + 1) // Generate a random length
            |> randomFloatArray rng maxRateValue
    } |> Array.ofSeq

// Generate possible array<BufferState> to be used as the BufferStates field
let buffers =
    seq {
        for _ in 1 .. buffersCount ->
            ((rng.Next maxRatesLength) + 1)
            |> randomBuffersArray rng
    } |> Array.ofSeq


// We want to make sure that all of our versions of the Settings type
// have the same underlying data to hash and compare. This means we
// need to compute the indices for the underlying data and use them
// for all the versions of the Settings type we create.
let valueIndexes =
    seq {
        for _ in 1 .. randomSettingsCount ->
        {|
            LevelsIdx = rng.Next (0, levels.Length)
            MaxRatesIdx = rng.Next (0, maxRates.Length)
            BufferStatesIdx = rng.Next (0, buffers.Length)
        |}
    } |> Array.ofSeq
    
// These will be the indices for deciding which Settings values we
// will look up in each of the dictionary. We want to ensure we are
// looking up equivalent data in all the tests.
let testIndexes =
    seq {
        for _ in 1 .. lookupCount ->
            rng.Next (0, randomSettingsCount)
    } |> Array.ofSeq

// Types and data for the default F# behavior
module Default =
    
    type Settings =
        {
            Levels : array<float>
            MaxRates : array<float>
            Buffers : array<BufferState>
        }
    
    // We now generate the random Settings we will be using
    let settings =
        seq {
            for vi in valueIndexes ->
            {
                Levels = levels.[vi.LevelsIdx]
                MaxRates = maxRates.[vi.MaxRatesIdx]
                Buffers = buffers.[vi.BufferStatesIdx]
            }
        } |> Array.ofSeq


    // The values we will test looking up in a Dictionary
    let settingsKeys =
        testIndexes
        |> Array.map (fun idx -> settings.[idx])
        
    // Create the dictionary for looking up Settings
    let settingsDictionary =
        settings
        |> Array.mapi (fun i settings -> KeyValuePair (settings, i))
        |> Dictionary
    
// Types and data for the Simple overrides
module Simple =
       
    [<CustomEquality; NoComparison>]
    type Settings =
        {
            Levels : array<float>
            MaxRates : array<float>
            Buffers : array<BufferState>
        }
        override this.Equals b =
            match b with
            | :? Settings as other ->
                this.Levels = other.Levels
                && this.MaxRates = other.MaxRates
                && this.Buffers = other.Buffers
            | _ -> false
            
        override this.GetHashCode () =
            hash (this.Levels, this.MaxRates, this.Buffers)
       
    // We now generate the random SimpleSettings we will be using
    let settings =
        seq {
            for vi in valueIndexes ->
            {
                Levels = levels.[vi.LevelsIdx]
                MaxRates = maxRates.[vi.MaxRatesIdx]
                Buffers = buffers.[vi.BufferStatesIdx]
            }
        } |> Array.ofSeq
       
    // The values we will test looking up in a Dictionary
    let settingsKeys =
        testIndexes
        |> Array.map (fun idx -> settings.[idx])
        
    // Create the dictionary for looking up Settings
    let settingsDictionary =
        settings
        |> Array.mapi (fun i settings -> KeyValuePair (settings, i))
        |> Dictionary
        
    
module FloatHash =
    
    let inline HashCombine nr x y = (x <<< 1) + y + 631 * nr
    
    let HashFloatArray (x: array<float>) : int =
          let len = x.Length
          let mutable i = len - 1 
          let mutable acc = 0
          while (i >= 0) do 
              acc <- HashCombine i acc (int x.[i])
              i <- i - 1
          acc
          
    [<CustomEquality; NoComparison>]
    type Settings =
        {
            Levels : array<float>
            MaxRates : array<float>
            Buffers : array<BufferState>
        }
        override this.Equals b =
            match b with
            | :? Settings as other ->
                this.Levels = other.Levels
                && this.MaxRates = other.MaxRates
                && this.Buffers = other.Buffers
            | _ -> false
            
        override this.GetHashCode () =
            let levelsHash = HashFloatArray this.Levels
            let maxRatesHash = HashFloatArray this.MaxRates
            let buffersHash = this.Buffers.GetHashCode()
            hash (levelsHash, maxRatesHash, buffersHash)
            
    // We now generate the random SimpleSettings we will be using
    let settings =
        seq {
            for vi in valueIndexes ->
            {
                Levels = levels.[vi.LevelsIdx]
                MaxRates = maxRates.[vi.MaxRatesIdx]
                Buffers = buffers.[vi.BufferStatesIdx]
            }
        } |> Array.ofSeq
       
    // The values we will test looking up in a Dictionary
    let settingsKeys =
        testIndexes
        |> Array.map (fun idx -> settings.[idx])
        
    // Create the dictionary for looking up Settings
    let settingsDictionary =
        settings
        |> Array.mapi (fun i settings -> KeyValuePair (settings, i))
        |> Dictionary
  
  
module FloatHashShort =
    
    let defaultHashNodes = 18
    let inline HashCombine nr x y = (x <<< 1) + y + 631 * nr
    
    let HashFloatArray (x: array<float>) : int =
          let len = x.Length
          let mutable i = len - 1
          if i > defaultHashNodes then i <- defaultHashNodes // limit the hash
          let mutable acc = 0
          while (i >= 0) do 
              acc <- HashCombine i acc (int x.[i])
              i <- i - 1
          acc
          
    [<CustomEquality; NoComparison>]
    type Settings =
        {
            Levels : array<float>
            MaxRates : array<float>
            Buffers : array<BufferState>
        }
        override this.Equals b =
            match b with
            | :? Settings as other ->
                this.Levels = other.Levels
                && this.MaxRates = other.MaxRates
                && this.Buffers = other.Buffers
            | _ -> false
            
        override this.GetHashCode () =
            let levelsHash = HashFloatArray this.Levels
            let maxRatesHash = HashFloatArray this.MaxRates
            let buffersHash = this.Buffers.GetHashCode()
            hash (levelsHash, maxRatesHash, buffersHash)
            
    // We now generate the random SimpleSettings we will be using
    let settings =
        seq {
            for vi in valueIndexes ->
            {
                Levels = levels.[vi.LevelsIdx]
                MaxRates = maxRates.[vi.MaxRatesIdx]
                Buffers = buffers.[vi.BufferStatesIdx]
            }
        } |> Array.ofSeq
       
    // The values we will test looking up in a Dictionary
    let settingsKeys =
        testIndexes
        |> Array.map (fun idx -> settings.[idx])
        
    // Create the dictionary for looking up Settings
    let settingsDictionary =
        settings
        |> Array.mapi (fun i settings -> KeyValuePair (settings, i))
        |> Dictionary
        
// Type to contain our performance tests
type Benchmarks () =

    [<Benchmark>]
    member _.Default () =
        let mutable idx = 0
        let mutable result = 0

        while idx < Default.settingsKeys.Length do
            let testKey = Default.settingsKeys.[idx]
            result <- Default.settingsDictionary.[testKey]

            idx <- idx + 1

        result
        
    [<Benchmark>]
    member _.Simple () =
        let mutable idx = 0
        let mutable result = 0

        while idx < Simple.settingsKeys.Length do
            let testKey = Simple.settingsKeys.[idx]
            result <- Simple.settingsDictionary.[testKey]

            idx <- idx + 1

        result
        
    [<Benchmark>]
    member _.FloatHash () =
        let mutable idx = 0
        let mutable result = 0

        while idx < FloatHash.settingsKeys.Length do
            let testKey = FloatHash.settingsKeys.[idx]
            result <- FloatHash.settingsDictionary.[testKey]

            idx <- idx + 1

        result
        
    [<Benchmark>]
    member _.FloatHashShort () =
        let mutable idx = 0
        let mutable result = 0

        while idx < FloatHashShort.settingsKeys.Length do
            let testKey = FloatHashShort.settingsKeys.[idx]
            result <- FloatHashShort.settingsDictionary.[testKey]

            idx <- idx + 1

        result
    
    
    
    
[<EntryPoint>]
let main argv =  
    
    let summary = BenchmarkRunner.Run<Benchmarks>()
    
    0

