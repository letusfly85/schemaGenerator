{-
 - ghc --make -optl-mwindows generateSchemaScripts.hs -o genSchemaTxt
 -}

import Graphics.UI.Gtk
import qualified DBAccessor as DB

main = do
    initGUI
    window <- windowNew

    set window [windowTitle          := "Generate [CREATE USER] Scripts",
                containerBorderWidth := 10,
                windowDefaultWidth   := 400,
                windowDefaultHeight  := 250]

    widgetModifyBg window StateNormal (Color 0 39321 26214)
    widgetShapeCombineMask window Nothing 0 0

    vb <- vBoxNew False 0
    containerAdd window vb

{-
 - schama name entry
 -}
    vbSchamaName <- vBoxNew False 0
    boxPackStart vb vbSchamaName PackNatural 0
    widgetShapeCombineMask vbSchamaName Nothing 100 100

    msg <- labelNew (Just "enter the schama name")
    boxPackStart vbSchamaName msg PackNatural 0

    txtEntry <- entryNew
    boxPackStart vbSchamaName txtEntry PackNatural 5

    sep1 <- hSeparatorNew
    boxPackStart vb sep1 PackNatural 7

{-
 - grant list and quota list selection
 -}
    hbOptionList <- hBoxNew False 0
    containerAdd vb hbOptionList

-- grant list
    vbGrantList <- vBoxNew False 0
    boxPackStart hbOptionList vbGrantList PackNatural 0

    gMsg <- labelNew (Just "select the grant number")
    boxPackStart vbGrantList gMsg PackNatural 0

    gCombo <- comboBoxNewText
    boxPackStart vbGrantList gCombo PackNatural 5

    grantAliasList <- DB.getGrantAliasList :: IO [(Int,String)]
    comboBoxSetAlias gCombo grantAliasList

    sep1_1 <- vSeparatorNew
    boxPackStart hbOptionList sep1_1 PackNatural 10

-- quota list
    vbQuotaList <- vBoxNew False 0
    boxPackStart hbOptionList vbQuotaList PackNatural 0

    qMsg <- labelNew (Just "select the quota number")
    boxPackStart vbQuotaList qMsg PackNatural 0

    qCombo <- comboBoxNewText
    boxPackStart vbQuotaList qCombo PackNatural 5

    quotaAliasList <- DB.getQuotaAliasList
    comboBoxSetAlias qCombo quotaAliasList

    sep2 <- hSeparatorNew
    boxPackStart vb sep2 PackNatural 0

{-
 - chose direcotry to put out scripts 
 -}
    vbDirectory <- vBoxNew False 0
    containerAdd vb vbDirectory

    dMsg <- labelNew (Just "select the output folder")
    boxPackStart vbDirectory dMsg PackNatural 0

    dChoisebutton <- fileChooserButtonNew "Select Folder" FileChooserActionSelectFolder
    boxPackStart vbDirectory dChoisebutton PackNatural 5

    sep3 <- hSeparatorNew
    boxPackStart vb sep3 PackNatural 0

{-
 - submit button
 -}
    vbDecision <- vBoxNew False 0
    containerAdd vb vbDecision

    dcMsg <- labelNew (Just "click to get scripts")
    boxPackStart vbDecision dcMsg PackNatural 0

    button <- buttonNewWithLabel "generate"
    boxPackStart vbDecision button PackNatural 5

    onClicked button (generateScripts txtEntry gCombo qCombo dChoisebutton)

    widgetShowAll window
    onDestroy window mainQuit
    mainGUI

{-
 - get filepath
 -}
getScriptsFilePath :: FileChooserButton -> IO FilePath
getScriptsFilePath dChoisebutton = do
    flg <- fileChooserGetFilename dChoisebutton
    case flg of
         Just f  -> return f
         Nothing -> return "."

getComboBoxActiveText :: ComboBox -> IO String
getComboBoxActiveText cm = do
    flg <- comboBoxGetActiveText cm
    case flg of
         Just str -> return str
         Nothing  -> return ""

generateScripts :: Entry -> ComboBox -> ComboBox -> FileChooserButton -> IO ()
generateScripts txtEntry gCombo qCombo dChoisebutton = do
    schemaName        <- entryGetText txtEntry
    grantAliasName    <- getComboBoxActiveText gCombo
    quotaKind         <- getComboBoxActiveText qCombo
    directoryName <- getScriptsFilePath dChoisebutton
    DB.generateAllScripts schemaName grantAliasName quotaKind directoryName
    
{-
 - set combobox dynamically
 -}
comboBoxSetAlias :: ComboBox -> [(Int,String)] -> IO ()
comboBoxSetAlias qCombo []     = return ()
comboBoxSetAlias qCombo (x:xs) = do
    comboBoxInsertText qCombo (fst x) (snd x)
    comboBoxSetAlias qCombo  xs
