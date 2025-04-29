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
import co.blocke.edi4j.model4j.refined.RefinedDocumentSpec;


public class FileReader {
    /**
     * Reads the whole file at `filePath` into one String (including line breaks).
     */
    public static String readFileToString(String filePath) throws IOException {
        return Files.readString(Path.of(filePath));
    }

    public static String convertSpec( String filePath ) throws IOException, CanonicalError {
        String ediStr = readFileToString(filePath);
        RefinedDocumentSpec rds = CanonicalParser.toRefined(fromJson(ediStr), "TS856", "856", "5010", "ANSI");
        return toJson(rds);
    }

    static SimpleModule enumOrSchemaModule = new SimpleModule()
            .addDeserializer(EnumOrSchema.class, new EnumOrSchemaDeserializer());
    static SimpleModule propertyModule     = new SimpleModule()
            .addDeserializer(Property      .class, new PropertyDeserializer());

    private static final ObjectMapper MAPPER = new ObjectMapper()
            // <-- infers constructor parameter names (requires javac -parameters)
            .registerModule(new ParameterNamesModule())
            // <-- adds support for java.util.Optional
            .registerModule(new Jdk8Module())
            .registerModule(enumOrSchemaModule)
            .registerModule(propertyModule)
            // ignore any extra JSON properties you don’t have in your model
            .configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false)
            .setSerializationInclusion(JsonInclude.Include.NON_ABSENT)
            .enable(MapperFeature.INFER_CREATOR_FROM_CONSTRUCTOR_PROPERTIES);

    /** Deserialize JSON into your top‐level EdiObject */
    public static EdiObject fromJson(String json) throws IOException {
        return MAPPER.readValue(json, EdiObject.class);
    }
    public static String toJson(RefinedDocumentSpec refinedObject) throws IOException {
        return MAPPER.writeValueAsString(refinedObject);
    }

}
