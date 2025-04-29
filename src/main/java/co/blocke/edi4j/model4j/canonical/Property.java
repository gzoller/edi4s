// Property.java
package co.blocke.edi4j.model4j.canonical;

import java.util.Map;

public interface Property {

    /** May throw if reference not found or wrong type */
    EnumOrSchema dereference(Map<String, EnumOrSchema> schemas) throws CanonicalError;

    /** Same as dereference, but must yield an EdiSchema */
    default EdiSchema dereferenceSegment(Map<String, EnumOrSchema> schemas) throws CanonicalError {
        EnumOrSchema resolved = dereference(schemas);
        if (resolved instanceof EdiSchema) {
            return (EdiSchema) resolved;
        }
        throw new CanonicalError("Expecting EdiSchema but found EdiEnum");
    }
}

