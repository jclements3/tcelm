-- Simple Double-Entry Accounting Ledger
-- Tests: custom types, records, do-notation, Dict, Result

module Ledger exposing (main)


-- Account Types

type AccountType
    = Asset
    | Liability
    | Equity
    | Revenue
    | Expense


type alias Account =
    { id : Int
    , name : String
    , accountType : AccountType
    , balance : Int  -- in cents to avoid floating point
    }


-- Transaction Types

type alias TransactionEntry =
    { accountId : Int
    , amount : Int  -- positive = debit, negative = credit
    }


type alias Transaction =
    { id : Int
    , description : String
    , entries : List TransactionEntry
    }


-- Ledger State

type alias Ledger =
    { accounts : Dict.Dict Int Account
    , transactions : List Transaction
    , nextAccountId : Int
    , nextTransactionId : Int
    }


-- Validation

validateBalanced : List TransactionEntry -> Result String ()
validateBalanced entries =
    let
        total = List.foldl (\e acc -> acc + e.amount) 0 entries
    in
    if total == 0 then
        Ok ()
    else
        Err "Transaction entries must sum to zero"


validateAccountsExist : Ledger -> List TransactionEntry -> Result String ()
validateAccountsExist ledger entries =
    let
        accounts = ledger.accounts

        check : TransactionEntry -> Result String ()
        check entry =
            let
                entryAccountId = entry.accountId
            in
            case Dict.get entryAccountId accounts of
                Just _ -> Ok ()
                Nothing -> Err ("Account not found: " ++ String.fromInt entryAccountId)
    in
    List.foldl
        (\entry acc ->
            case acc of
                Err e -> Err e
                Ok _ -> check entry
        )
        (Ok ())
        entries


-- Operations

emptyLedger : Ledger
emptyLedger =
    { accounts = Dict.empty
    , transactions = []
    , nextAccountId = 1
    , nextTransactionId = 1
    }


createAccount : String -> AccountType -> Ledger -> ( Account, Ledger )
createAccount name accountType ledger =
    let
        accountId = ledger.nextAccountId
        currentAccounts = ledger.accounts

        account =
            { id = accountId
            , name = name
            , accountType = accountType
            , balance = 0
            }

        newLedger =
            { ledger
            | accounts = Dict.insert accountId account currentAccounts
            , nextAccountId = accountId + 1
            }
    in
    ( account, newLedger )


postTransaction : String -> List TransactionEntry -> Ledger -> Result String Ledger
postTransaction description entries ledger =
    do
        _ <- validateBalanced entries
        _ <- validateAccountsExist ledger entries
        let
            currentAccounts = ledger.accounts
            txnId = ledger.nextTransactionId
            currentTransactions = ledger.transactions

            -- Apply each entry to update account balances
            updatedAccounts =
                List.foldl
                    (\entry accs ->
                        let
                            entryAccountId = entry.accountId
                            entryAmount = entry.amount
                        in
                        Dict.update entryAccountId
                            (\maybeAccount ->
                                case maybeAccount of
                                    Just acc ->
                                        let
                                            currentBalance = acc.balance
                                        in
                                        Just { acc | balance = currentBalance + entryAmount }
                                    Nothing ->
                                        Nothing
                            )
                            accs
                    )
                    currentAccounts
                    entries

            transaction =
                { id = txnId
                , description = description
                , entries = entries
                }
        Ok
            { ledger
            | accounts = updatedAccounts
            , transactions = transaction :: currentTransactions
            , nextTransactionId = txnId + 1
            }


getAccountBalance : Account -> Int
getAccountBalance acc = acc.balance

getBalance : Int -> Ledger -> Maybe Int
getBalance accountId ledger =
    let
        accounts = ledger.accounts
    in
    Dict.get accountId accounts
        |> Maybe.map getAccountBalance


-- Example usage

main : Int
main =
    let
        -- Create accounts
        ( cashAccount, ledger1 ) = createAccount "Cash" Asset emptyLedger
        ( revenueAccount, ledger2 ) = createAccount "Revenue" Revenue ledger1

        -- Post a transaction: receive $100 cash (debit Cash, credit Revenue)
        entries =
            [ { accountId = cashAccount.id, amount = 10000 }  -- debit cash +$100
            , { accountId = revenueAccount.id, amount = -10000 }  -- credit revenue -$100
            ]

        result = postTransaction "Initial sale" entries ledger2
    in
    case result of
        Ok finalLedger ->
            -- Return cash balance (should be 10000 cents = $100)
            getBalance cashAccount.id finalLedger
                |> Maybe.withDefault 0

        Err _ ->
            -1
