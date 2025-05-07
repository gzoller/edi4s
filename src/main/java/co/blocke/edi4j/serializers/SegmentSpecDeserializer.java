package co.blocke.edi4j.serializers;

import com.fasterxml.jackson.core.*;
import com.fasterxml.jackson.core.ObjectCodec;
import com.fasterxml.jackson.databind.*;
import com.fasterxml.jackson.databind.node.ObjectNode;
import co.blocke.edi4j.model4j.refined.*;

import java.io.IOException;

public class SegmentSpecDeserializer
        extends JsonDeserializer<RefinedSingleOrLoopSegmentSpec> {

    @Override
    public RefinedSingleOrLoopSegmentSpec deserialize(JsonParser p,
                                                      DeserializationContext ctxt)
            throws IOException, JsonProcessingException {
        ObjectCodec codec = p.getCodec();
        ObjectNode node  = codec.readTree(p);

        // if JSON has a "body" array → it's a loop spec
        if (node.has("body")) {
            return codec.treeToValue(node, RefinedLoopSpec.class);
        }
        // otherwise it's a single‐segment spec
        return codec.treeToValue(node, RefinedSegmentSpec.class);
    }
}
