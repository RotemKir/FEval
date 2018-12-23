namespace FEval

module Inspections =
    
    open FEval.EvaluationTypes
    open FEval.Inspectors

    type InspectionType =
    | Performance
    | MethodCalls
    | SettingValues

    type InspectionLogging<'a> =
    | LogToTextFile of string
    | LogToCsvFile of string
       
    let inspectionOf inspectionType logger =
        match (inspectionType, logger) with
        | (Performance, LogToTextFile fileName) 
            -> PerformanceInspector.createNew <| PerformanceInspector.createTxtLogger fileName
        | (Performance, LogToCsvFile fileName) 
            -> PerformanceInspector.createNew <| PerformanceInspector.createCsvLogger fileName
        | (MethodCalls, LogToTextFile fileName) 
            -> MethodCallInspector.createNew <| MethodCallInspector.createTxtLogger fileName
        | (MethodCalls, LogToCsvFile fileName) 
            -> MethodCallInspector.createNew <| MethodCallInspector.createCsvLogger fileName
        | (SettingValues, LogToTextFile fileName) 
            -> SetValueInspector.createNew <| SetValueInspector.createTxtLogger fileName
        | (SettingValues, LogToCsvFile fileName) 
            -> SetValueInspector.createNew <| SetValueInspector.createCsvLogger fileName