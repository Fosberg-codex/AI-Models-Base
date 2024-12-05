import HashMap "mo:base/HashMap";
import Text "mo:base/Text";
import Nat "mo:base/Nat";
import Result "mo:base/Result";
import Array "mo:base/Array";
import Buffer "mo:base/Buffer";
import Time "mo:base/Time";
import Iter "mo:base/Iter";
import Order "mo:base/Order";
import Option "mo:base/Option";

actor {
  // Define the Model type
  public type Model = {
    id: Text;
    name: Text;
    description: Text;
    modelType: ModelType;
    githubLink: Text;
    articleLink: ?Text;
    submitterLinkedIn: Text;
    createdAt: Int;
  };

  // Define the model type enum
  public type ModelType = {
    #NLP;
    #TabularData;
    #ComputerVision;
    #SpeechRecognition;
    #Other;
  };

  // Create a stable HashMap to store models
  stable var modelEntries : [(Text, Model)] = [];
  let models = HashMap.HashMap<Text, Model>(0, Text.equal, Text.hash);

  // Counter for generating unique IDs
  stable var nextId : Nat = 0;

  // Generate a unique ID
  func generateUniqueId() : Text {
    nextId += 1;
    return Nat.toText(nextId);
  };

  // Create a new model
  public shared(msg) func createModel(
    name : Text, 
    description : Text, 
    modelType : ModelType, 
    githubLink : Text, 
    articleLink : ?Text, 
    submitterLinkedIn : Text
  ) : async Model {
    let id = generateUniqueId();
    let newModel : Model = {
      id = id;
      name = name;
      description = description;
      modelType = modelType;
      githubLink = githubLink;
      articleLink = articleLink;
      submitterLinkedIn = submitterLinkedIn;
      createdAt = Time.now();
    };
    models.put(id, newModel);
    return newModel;
  };

  // Get all models
  public query func getAllModels() : async [Model] {
    let buffer = Buffer.Buffer<Model>(0);
    for (model in models.vals()) {
      buffer.add(model);
    };
    return Array.sort<Model>(
      Buffer.toArray(buffer),
      func(a: Model, b: Model) : Order.Order { 
        if (a.createdAt < b.createdAt) { #less } 
        else if (a.createdAt > b.createdAt) { #greater } 
        else { #equal }
      }
    );
  };

  // Get models by type
  public query func getModelsByType(modelType : ModelType) : async [Model] {
    let buffer = Buffer.Buffer<Model>(0);
    for (model in models.vals()) {
      if (model.modelType == modelType) {
        buffer.add(model);
      }
    };
    return Buffer.toArray(buffer);
  };

  // Update a model
  public shared(msg) func updateModel(
    id : Text, 
    name : ?Text, 
    description : ?Text, 
    modelType : ?ModelType, 
    githubLink : ?Text, 
    articleLink : ?Text, 
    submitterLinkedIn : ?Text
  ) : async ?Model {
    switch (models.get(id)) {
      case null { null };
      case (?existingModel) {
        let updatedModel : Model = {
          id = existingModel.id;
          name = Option.get(name, existingModel.name);
          description = Option.get(description, existingModel.description);
          modelType = Option.get(modelType, existingModel.modelType);
          githubLink = Option.get(githubLink, existingModel.githubLink);
          articleLink = switch (articleLink) {
            case null { existingModel.articleLink };
            case (?link) { ?link };
          };
          submitterLinkedIn = Option.get(submitterLinkedIn, existingModel.submitterLinkedIn);
          createdAt = existingModel.createdAt;
        };
        models.put(id, updatedModel);
        ?updatedModel;
      };
    };
  };

  // Delete a model
  public shared(msg) func deleteModel(id : Text) : async Bool {
    switch (models.get(id)) {
      case null { false };
      case (?model) {
        models.delete(id);
        true;
      };
    };
  };

  // Preupgrade and postupgrade hooks to maintain state
  system func preupgrade() {
    modelEntries := Iter.toArray(models.entries());
  };

  system func postupgrade() {
    for ((id, model) in modelEntries.vals()) {
      models.put(id, model);
    };
    modelEntries := [];
  };
}




//actor {
//  public query func greet(name : Text) : async Text {
//    return "Hello, " # name # "!";
//  };
//};
