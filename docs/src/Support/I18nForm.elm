module Support.I18nForm exposing (Model, Msg, init, update, view)

import Browser
import Countries
import FormToolkit.Error as Error exposing (Error)
import FormToolkit.Field as Field exposing (Field)
import FormToolkit.Parse as Parse
import FormToolkit.Value as Value
import FormToolkit.View as View
import Html exposing (Html)
import Html.Attributes as Attr exposing (novalidate)
import Html.Events exposing (onClick, onSubmit)


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- TYPES


type alias Model =
    { shipmentFields : Field ShipmentFields
    , submitted : Bool
    , result : Maybe (Result (Error ShipmentFields) Shipment)
    }


type alias Shipment =
    { shipping : Address
    }


type alias Address =
    { firstName : String
    , lastName : String
    , address : String
    , addressNumber : Int
    , addressExtra : Maybe String
    , postalCode : String
    , state : String
    , country : Countries.Country
    }


type ShipmentFields
    = AddressNameGroup
    | AddressFirstName
    | AddressLastName
    | AddressStreet
    | AddressNumber
    | AddressExtra
    | LocalityGroup
    | PostalCode
    | AddressState
    | AddressCountry


type Msg
    = FormChanged (Field.Msg ShipmentFields)
    | FormSubmitted



-- INIT


init : Model
init =
    { shipmentFields = shipmentFieldsDefinition
    , submitted = False
    , result = Nothing
    }



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case msg of
        FormChanged inputMsg ->
            let
                ( shipmentFields, result ) =
                    Parse.parseUpdate shipmentParser inputMsg model.shipmentFields
            in
            { shipmentFields = shipmentFields
            , submitted = False
            , result = Just result
            }

        FormSubmitted ->
            { model
                | submitted = True
                , shipmentFields =
                    model.shipmentFields
                        |> Field.validate
                        |> Field.touch
            }



-- FORM DEFINITION (Spanish)


shipmentFieldsDefinition : Field ShipmentFields
shipmentFieldsDefinition =
    Field.group
        []
        [ Field.group
            [ Field.label "Destinatario"
            , Field.name "recipient"
            , Field.identifier AddressNameGroup
            , Field.class "inline-fields"
            ]
            [ Field.text
                [ Field.label "Nombre"
                , Field.identifier AddressFirstName
                , Field.name "first-name"
                , Field.required True
                ]
            , Field.text
                [ Field.label "Apellido"
                , Field.identifier AddressLastName
                , Field.name "last-name"
                , Field.required True
                ]
            ]
        , Field.group
            [ Field.name "address"
            , Field.label "Dirección"
            ]
            [ Field.group
                [ Field.class "inline-fields" ]
                [ Field.text
                    [ Field.label "Nombre de la Calle"
                    , Field.class "column column-75"
                    , Field.required True
                    , Field.identifier AddressStreet
                    , Field.name "street-name"
                    ]
                , Field.text
                    [ Field.label "Número"
                    , Field.identifier AddressNumber
                    , Field.required True
                    , Field.name "address-number"
                    ]
                ]
            , Field.text
                [ Field.label "Dirección 2"
                , Field.identifier AddressExtra
                , Field.name "address-2"
                ]
            , Field.group
                [ Field.identifier LocalityGroup
                , Field.class "locality"
                , Field.class "inline-fields"
                ]
                [ Field.text
                    [ Field.label "Código Postal"
                    , Field.required True
                    , Field.identifier PostalCode
                    , Field.name "postal-code"
                    ]
                , Field.text
                    [ Field.label "Provincia"
                    , Field.required True
                    , Field.identifier AddressState
                    , Field.name "state"
                    ]
                , Field.select
                    [ Field.label "País"
                    , Field.required True
                    , Field.identifier AddressCountry
                    , Field.name "country"
                    , Field.options
                        (Countries.all
                            |> List.map
                                (\country ->
                                    ( country.name ++ " " ++ country.flag
                                    , Value.string country.code
                                    )
                                )
                        )
                    ]
                ]
            ]
        ]



-- PARSER


shipmentParser : Parse.Parser ShipmentFields Shipment
shipmentParser =
    Parse.map Shipment
        shipmentAddressParser


shipmentAddressParser : Parse.Parser ShipmentFields Address
shipmentAddressParser =
    Parse.succeed Address
        |> Parse.andMap (Parse.field AddressFirstName Parse.string)
        |> Parse.andMap (Parse.field AddressLastName Parse.string)
        |> Parse.andMap (Parse.field AddressStreet Parse.string)
        |> Parse.andMap (Parse.field AddressNumber Parse.int)
        |> Parse.andMap (Parse.field AddressExtra (Parse.maybe Parse.string))
        |> Parse.andMap (Parse.field PostalCode Parse.string)
        |> Parse.andMap (Parse.field AddressState Parse.string)
        |> Parse.andMap shipmentCountryParser


shipmentCountryParser : Parse.Parser ShipmentFields Countries.Country
shipmentCountryParser =
    Parse.field AddressCountry
        (Parse.string
            |> Parse.andThen
                (\countryStr ->
                    case Countries.fromCode countryStr of
                        Just country ->
                            Parse.succeed country

                        Nothing ->
                            Parse.fail "País inválido"
                )
        )



-- VIEW WITH SPANISH TRANSLATIONS


view : Model -> Html Msg
view model =
    Html.div
        [ Attr.class "milligram"
        , Attr.style "margin-top" "20px"
        , Attr.style "padding" "20px"
        , Attr.style "border" "1px solid #d1d1d1"
        , Attr.style "border-radius" "4px"
        ]
        [ Html.form
            [ onSubmit FormSubmitted, novalidate True ]
            [ model.shipmentFields
                |> View.fromField FormChanged
                |> View.customizeErrors errorToSpanish
                |> View.toHtml
            , Html.button
                [ onClick FormSubmitted
                , Attr.style "margin-top" "1rem"
                ]
                [ Html.text "Enviar" ]
            ]
        , if model.submitted then
            case model.result of
                Just (Ok shipment) ->
                    success
                        [ Html.div
                            []
                            [ Html.text "¡El formulario fue correctamente llenado!" ]
                        , Html.div
                            []
                            [ Html.text
                                ("Envío procesado para: " ++ shipment.shipping.firstName ++ " " ++ shipment.shipping.lastName)
                            ]
                        ]

                Just (Err _) ->
                    failure
                        [ Html.text "Por favor revise los errores"
                        ]

                Nothing ->
                    Html.text ""

          else
            Html.text ""
        ]


errorToSpanish : View.FieldAttributes ShipmentFields -> String
errorToSpanish attributes =
    let
        toString =
            Value.toString >> Maybe.withDefault ""

        errorToString error =
            case error of
                Error.IsBlank _ ->
                    "Este campo es obligatorio"

                Error.ValueTooSmall _ data ->
                    "El valor mínimo es " ++ toString data.min

                Error.ValueTooLarge _ data ->
                    "El valor máximo es " ++ toString data.max

                Error.ValueNotInRange _ data ->
                    "El valor debe estar entre " ++ toString data.min ++ " y " ++ toString data.max

                Error.NotNumber _ ->
                    "Debe ser un número"

                Error.NotBool _ ->
                    "Debe ser verdadero o falso"

                Error.HasNoName _ ->
                    "No se pudo procesar"

                Error.PatternError _ ->
                    "Formato inválido"

                Error.EmailInvalid _ ->
                    "Debe ser una dirección de correo válida"

                Error.UrlInvalid _ ->
                    "Debe ser una URL válida"

                Error.IsGroupNotInput _ ->
                    "Error en el grupo"

                Error.NoOptionsProvided _ ->
                    "No hay opciones disponibles"

                Error.InvalidValue _ ->
                    "La selección no es válida"

                Error.InputNotFound _ ->
                    "Campo no encontrado"

                Error.ErrorList _ errors ->
                    "Errores: " ++ String.join ", " (List.map errorToString errors)

                Error.ParseError _ ->
                    "No se pudo procesar"

                Error.CustomError _ message ->
                    message
    in
    attributes.errors
        |> List.map errorToString
        |> String.join ", "


success =
    Html.div
        [ Attr.style "margin-top" "1rem"
        , Attr.style "padding" "1rem"
        , Attr.style "background" "#e8f5e8"
        , Attr.style "border-radius" "4px"
        ]


failure =
    Html.div
        [ Attr.style "margin-top" "1rem"
        , Attr.style "padding" "1rem"
        , Attr.style "background" "#fde8e8"
        , Attr.style "border-radius" "4px"
        ]
