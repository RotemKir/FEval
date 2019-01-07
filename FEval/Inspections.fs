namespace FEval

open FEval.Inspectors.ValidationsCommon
open FEval.Inspectors.ValidationRules

module Inspections =
    
    open FEval.Inspectors

    type InspectionType =
    | Performance
    | MethodCalls
    | SettingValues
    | Validation of RuleDefinition seq

    type InspectionLogging =
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
        | (Validation rules, LogToTextFile fileName) 
            -> ValidationInspector.createNew rules <| ValidationInspector.createTxtLogger fileName
        | (Validation rules, LogToCsvFile fileName) 
            -> ValidationInspector.createNew rules <| ValidationInspector.createCsvLogger fileName
    
    let ifVariable name invalidWhen thenReturn =
        VariableRule
            {
                VariableName = name
                Validation = getVariableValidation invalidWhen
                ReturnWhenInvalid = thenReturn
            }