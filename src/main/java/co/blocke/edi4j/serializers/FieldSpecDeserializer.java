package co.blocke.edi4j.serializers;

import com.fasterxml.jackson.core.*;
import com.fasterxml.jackson.core.ObjectCodec;
import com.fasterxml.jackson.databind.*;
import com.fasterxml.jackson.databind.node.ObjectNode;
import co.blocke.edi4j.model4j.refined.*;

import java.io.IOException;

public class FieldSpecDeserializer
        extends JsonDeserializer<RefinedFieldSpec> {

    @Override
    public RefinedFieldSpec deserialize(JsonParser p,
                                        DeserializationContext ctxt)
            throws IOException, JsonProcessingException {
        ObjectCodec codec = p.getCodec();
        ObjectNode node  = codec.readTree(p);

        // If it has "components", it's a composite field spec
        if (node.has("components")) {
            return codec.treeToValue(node, RefinedCompositeFieldSpec.class);
        }
        // Otherwise it's a single-field spec
        return codec.treeToValue(node, RefinedSingleFieldSpec.class);
    }
}
