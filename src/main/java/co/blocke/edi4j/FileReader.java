package co.blocke.edi4j;

import co.blocke.edi4j.model4j.canonical.CanonicalError;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import com.fasterxml.jackson.annotation.*;
import com.fasterxml.jackson.databind.*;
import com.fasterxml.jackson.databind.module.SimpleModule;
import com.fasterxml.jackson.datatype.jdk8.Jdk8Module;
import com.fasterxml.jackson.module.paramnames.ParameterNamesModule;
import co.blocke.edi4j.serializers.*;
import co.blocke.edi4j.model4j.canonical.*;
import co.blocke.edi4j.model4j.refined.*;


public class FileReader {
    /**
     * Reads the whole file at `filePath` into one String (including line breaks).
     */
    public static String readFileToString(String filePath) throws IOException {
        return Files.readString(Path.of(filePath));
    }

    public static String convertSpec( String filePath ) throws IOException, CanonicalError {
        String ediStr = readFileToString(filePath);
        RefinedDocumentSpec rds = CanonicalParser.toRefined(ediFromJson(ediStr), "TS856", "856", "5010", "ANSI");
        return refinedToJson(rds);
    }

    public static RefinedDocumentSpec readRefined( String filePath ) throws IOException, CanonicalError {
        String refinedStr = readFileToString(filePath);
        return refinedFromJson(refinedStr);
    }

    static SimpleModule enumOrSchemaModule = new SimpleModule()
            .addDeserializer(EnumOrSchema.class, new EnumOrSchemaDeserializer());
    static SimpleModule propertyModule     = new SimpleModule()
            .addDeserializer(Property.class, new PropertyDeserializer());
    static SimpleModule segmentSpecModule = new SimpleModule()
            .addDeserializer(
                    RefinedSingleOrLoopSegmentSpec.class,
                    new SegmentSpecDeserializer()
            );
    static SimpleModule fieldSpecModule = new SimpleModule()
            .addDeserializer(
                    RefinedFieldSpec.class,
                    new FieldSpecDeserializer()
            );

    private static final ObjectMapper MAPPER = new ObjectMapper()
            // <-- infers constructor parameter names (requires javac -parameters)
            .registerModule(new ParameterNamesModule())
            // <-- adds support for java.util.Optional
            .registerModule(new Jdk8Module())
            .registerModule(enumOrSchemaModule)
            .registerModule(propertyModule)
            .registerModule(segmentSpecModule)
            .registerModule(fieldSpecModule)
            // ignore any extra JSON properties you don’t have in your model
            .configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false)
            .setSerializationInclusion(JsonInclude.Include.NON_ABSENT)
            .enable(MapperFeature.INFER_CREATOR_FROM_CONSTRUCTOR_PROPERTIES);

    /** Deserialize JSON into your top‐level EdiObject */
    public static EdiObject ediFromJson(String json) throws IOException {
        return MAPPER.readValue(json, EdiObject.class);
    }
    public static String refinedToJson(RefinedDocumentSpec refinedObject) throws IOException {
        return MAPPER.writeValueAsString(refinedObject);
    }
    public static RefinedDocumentSpec refinedFromJson(String json) throws IOException {
        return MAPPER.readValue(json, RefinedDocumentSpec.class);
    }

}
