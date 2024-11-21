import torch
import torch.nn as nn

class CognitiveLayerWrapper(nn.Module):
    def __init__(self, base_model, cognitive_vector, target_layer_num, multiplier=5):
        super().__init__()
        self.base_model = base_model

        self.cognitive_vector = nn.Parameter(cognitive_vector, requires_grad=True)
        self.multiplier = multiplier

        # Register a forward hook on the targeted layer
        layer_name = f'model.layers.{target_layer_num}'
        layer = dict(self.base_model.named_modules())[layer_name]
        layer.register_forward_hook(self.modify_hidden_states)

    def modify_hidden_states(self, module, inputs, output):
        if isinstance(output, tuple):
            hidden_states = output[0]
            cognitive_vector = self.cognitive_vector.to(hidden_states.dtype).unsqueeze(0).unsqueeze(0)
            modified_hidden_states = hidden_states + self.multiplier * cognitive_vector
            #print("Original Hidden States:", hidden_states)
            #print("Cognitive Vector:", self.cognitive_vector)
            #print("Modified Hidden States:", modified_hidden_states)
            return (modified_hidden_states,) + output[1:]
        else:
            cognitive_vector = self.cognitive_vector.to(output.dtype).unsqueeze(0).unsqueeze(0)
            modified_output = output + self.multiplier * cognitive_vector
            return modified_output

    def forward(self, inputs):
        input_ids = inputs.get('input_ids')
        attention_mask = inputs.get('attention_mask')
        outputs = self.base_model(input_ids=input_ids, attention_mask=attention_mask, output_hidden_states=True)
        return outputs.hidden_states[-1]

class CognitiveLLaMA(nn.Module):
    def __init__(self, base_model, cognitive_vector, target_layer_num, multiplier=5, dropout_prob=0.5):
        super().__init__()
        self.dropout = nn.Dropout(dropout_prob)
        self.cognitive_layer = CognitiveLayerWrapper(base_model, cognitive_vector, target_layer_num, multiplier)
        self.classifier = nn.Linear(base_model.config.hidden_size, 2)

    def forward(self, input_ids, attention_mask=None, labels=None):
        cognitive_output = self.cognitive_layer.forward({
            'input_ids': input_ids,
            'attention_mask': attention_mask
        })

        features = cognitive_output[:, -1, :]
        features = self.dropout(features)
        logits = self.classifier(features.float())

        if labels is None:
            probabilities = torch.softmax(logits, dim=-1)
            return logits, probabilities
        else:
            loss_fn = nn.CrossEntropyLoss()
            loss = loss_fn(logits, labels)
            return logits, loss
