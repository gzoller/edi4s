// EdiElementProperty.java
package co.blocke.edi4j.model4j.canonical;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import com.fasterxml.jackson.annotation.JsonProperty;

public class EdiElementProperty implements Property {
    private final String type;
    private final Optional<Integer> minLength;
    private final Optional<Integer> maxLength;
    private final Optional<String> format;
    private final Optional<List<String>> enums;
    private final Optional<List<Map<String,String>>> allOf;
    private final Optional<List<Map<String,String>>> anyOf;
    private final Optional<List<Map<String,String>>> oneOf;
    private final Optional<List<Map<String,String>>> not;
    private final Optional<String> xOpenediElementId;

    public EdiElementProperty(
        String type,
        Optional<Integer> minLength,
        Optional<Integer> maxLength,
        Optional<String> format,
        @JsonProperty("enum") Optional<List<String>> enums,
        Optional<List<Map<String,String>>> allOf,
        Optional<List<Map<String,String>>> anyOf,
        Optional<List<Map<String,String>>> oneOf,
        Optional<List<Map<String,String>>> not,
        @JsonProperty("x-openedi-element-id") Optional<String> xOpenediElementId
    ) {
        this.type             = type;
        this.minLength        = minLength;
        this.maxLength        = maxLength;
        this.format           = format;
        this.enums            = enums;
        this.allOf            = allOf;
        this.anyOf            = anyOf;
        this.oneOf            = oneOf;
        this.not              = not;
        this.xOpenediElementId= xOpenediElementId;
    }

    @Override
    public EnumOrSchema dereference(Map<String, EnumOrSchema> schemas) {
        // elements always deref to an empty‐props schema
        return new EdiSchema(
            Optional.empty(),
            type,
            new LinkedHashMap<>(),
            xOpenediElementId,
            Optional.empty(),
            Optional.empty(),
            Optional.empty()
        );
    }

    public String getType() {
        return type;
    }

    public Optional<Integer> getMinLength() {
        return minLength;
    }

    public Optional<Integer> getMaxLength() {
        return maxLength;
    }

    public Optional<String> getFormat() {
        return format;
    }

    public Optional<List<String>> getEnums() {
        return enums;
    }

    public Optional<List<Map<String, String>>> getAllOf() {
        return allOf;
    }

    public Optional<List<Map<String, String>>> getAnyOf() {
        return anyOf;
    }

    public Optional<List<Map<String, String>>> getOneOf() {
        return oneOf;
    }

    public Optional<List<Map<String, String>>> getNot() {
        return not;
    }

    public Optional<String> getxOpenediElementId() {
        return xOpenediElementId;
    }
}

