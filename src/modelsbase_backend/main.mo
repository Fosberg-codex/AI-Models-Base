import HashMap "mo:base/HashMap";
import Text "mo:base/Text";
import Nat "mo:base/Nat";
import Option "mo:base/Option";
import Array "mo:base/Array";
import Buffer "mo:base/Buffer";
import Time "mo:base/Time";
import Result "mo:base/Result";
import Principal "mo:base/Principal";
import Hash "mo:base/Hash";

actor {
  // Enum for Model Types
  type ModelType = {
    #Tabular;
    #ComputerVision;
    #NLP;
    #LLM;
    #VisionModel;
    #AudioModel;
    #Agents;
  };

  // Complexity Enum to indicate model sophistication
  type ModelComplexity = {
    #Beginner;
    #Intermediate;
    #Advanced;
    #Research;
  };

  // Licensing Options
  type LicenseType = {
    #OpenSource;
    #Commercial;
    #Academic;
    #ResearchOnly;
  };

  // Performance Metrics
  type PerformanceMetrics = {
    accuracy: ?Float;
    trainingTime: ?Text;
    modelSize: ?Text;
  };

  // Main Model Structure
  type AIModel = {
    id: Nat;
    name: Text;
    description: Text;
    modelType: ModelType;
    tags: [Text];
    link: Text;
    submitterName: Text;
    submitterLink: Text;
    complexity: ModelComplexity;
    licenseType: LicenseType;
    githubStars: ?Nat;
    paperLink: ?Text;
    publishedDate: Time.Time;
    frameworkUsed: ?Text;
    performanceMetrics: ?PerformanceMetrics;
  };

  // Input field titles for documentation and error messaging
  let inputTitles = {
    name = "Model Name";
    description = "Model Description";
    modelType = "AI Model Type";
    tags = "Model Tags";
    link = "Model Link/URL";
    submitterName = "Submitter's Name";
    submitterLink = "Submitter's Profile/Website";
    complexity = "Model Complexity Level";
    licenseType = "Licensing Type";
    githubStars = "GitHub Stars (Optional)";
    paperLink = "Research Paper Link (Optional)";
    frameworkUsed = "Machine Learning Framework (Optional)";
    performanceMetrics = "Performance Metrics (Optional)";
  };

  // Custom hash function for Nat
  func natHash(n: Nat) : Hash.Hash {
    return Text.hash(Nat.toText(n));
  };

  // Storage
  let models = HashMap.HashMap<Nat, AIModel>(10, Nat.equal, natHash);
  var nextId : Nat = 1;

  // Helper function to handle optional values
  func mergeOptional<T>(newValue : ?T, existingValue : T) : T {
    switch (newValue) {
      case null { existingValue };
      case (?value) { value };
    }
  };

  // Helper function to merge optional values with optional type
  func mergeOptionalFields<T>(newValue : ?T, existingValue : ?T) : ?T {
    switch (newValue) {
      case null { existingValue };
      case (?value) { ?value };
    }
  };

  // Create a new AI Model
  public func createModel(
    name: Text, 
    description: Text, 
    modelType: ModelType,
    tags: [Text],
    link: Text,
    submitterName: Text,
    submitterLink: Text,
    complexity: ModelComplexity,
    licenseType: LicenseType,
    githubStars: ?Nat,
    paperLink: ?Text,
    frameworkUsed: ?Text,
    performanceMetrics: ?PerformanceMetrics
  ) : async Result.Result<Nat, Text> {
    // Validate required fields with descriptive error messages
    if (name == "") {
      return #err("'" # inputTitles.name # "' cannot be empty");
    };

    if (link == "") {
      return #err("'" # inputTitles.link # "' cannot be empty");
    };

    let newModel : AIModel = {
      id = nextId;
      name = name;
      description = description;
      modelType = modelType;
      tags = tags;
      link = link;
      submitterName = submitterName;
      submitterLink = submitterLink;
      complexity = complexity;
      licenseType = licenseType;
      githubStars = githubStars;
      paperLink = paperLink;
      publishedDate = Time.now();
      frameworkUsed = frameworkUsed;
      performanceMetrics = performanceMetrics;
    };

    models.put(nextId, newModel);
    let modelId = nextId;
    nextId += 1;

    #ok(modelId)
  };

  // Read a specific model by ID
  public query func getModel(id: Nat) : async ?AIModel {
    models.get(id)
  };

  // Update an existing model
  public func updateModel(
    id: Nat, 
    name: ?Text, 
    description: ?Text,
    modelType: ?ModelType,
    tags: ?[Text],
    link: ?Text,
    submitterName: ?Text,
    submitterLink: ?Text,
    complexity: ?ModelComplexity,
    licenseType: ?LicenseType,
    githubStars: ?Nat,
    paperLink: ?Text,
    frameworkUsed: ?Text,
    performanceMetrics: ?PerformanceMetrics
  ) : async Result.Result<(), Text> {
    switch (models.get(id)) {
      case null { #err("Model not found") };
      case (?existingModel) {
        let updatedModel : AIModel = {
          id = existingModel.id;
          name = mergeOptional(name, existingModel.name);
          description = mergeOptional(description, existingModel.description);
          modelType = mergeOptional(modelType, existingModel.modelType);
          tags = mergeOptional(tags, existingModel.tags);
          link = mergeOptional(link, existingModel.link);
          submitterName = mergeOptional(submitterName, existingModel.submitterName);
          submitterLink = mergeOptional(submitterLink, existingModel.submitterLink);
          complexity = mergeOptional(complexity, existingModel.complexity);
          licenseType = mergeOptional(licenseType, existingModel.licenseType);
          githubStars = mergeOptionalFields(githubStars, existingModel.githubStars);
          paperLink = mergeOptionalFields(paperLink, existingModel.paperLink);
          publishedDate = existingModel.publishedDate;
          frameworkUsed = mergeOptionalFields(frameworkUsed, existingModel.frameworkUsed);
          performanceMetrics = mergeOptionalFields(performanceMetrics, existingModel.performanceMetrics);
        };

        models.put(id, updatedModel);
        #ok()
      };
    }
  };

  // Delete a model
  public func deleteModel(id: Nat) : async Result.Result<(), Text> {
    switch (models.get(id)) {
      case null { #err("Model not found") };
      case (?_) { 
        models.delete(id);
        #ok() 
      };
    }
  };

  // List all models
  public query func listModels() : async [(Nat, AIModel)] {
    let modelArray = Buffer.Buffer<(Nat, AIModel)>(models.size());
    
    for ((id, model) in models.entries()) {
      modelArray.add((id, model));
    };

    Buffer.toArray(modelArray)
  };

  // Search models by type
  public query func searchModelsByType(modelType: ModelType) : async [(Nat, AIModel)] {
    let filteredModels = Buffer.Buffer<(Nat, AIModel)>(0);
    
    for ((id, model) in models.entries()) {
      if (model.modelType == modelType) {
        filteredModels.add((id, model));
      }
    };

    Buffer.toArray(filteredModels)
  };

  // Search models by tag
  public query func searchModelsByTag(tag: Text) : async [(Nat, AIModel)] {
    let filteredModels = Buffer.Buffer<(Nat, AIModel)>(0);
    
    for ((id, model) in models.entries()) {
      if (Array.find(model.tags, func(t : Text) : Bool { t == tag }) != null) {
        filteredModels.add((id, model));
      }
    };

    Buffer.toArray(filteredModels)
  };
}







// Importing necessary modules
// import HashMap "mo:base/HashMap";
// import Text "mo:base/Text";
// import Nat "mo:base/Nat";
// import Option "mo:base/Option";
// import Array "mo:base/Array";
// import Buffer "mo:base/Buffer";
// import Time "mo:base/Time";
// import Result "mo:base/Result";
// import Principal "mo:base/Principal";
// import Hash "mo:base/Hash";

// Actor definition
// actor {
  // Enum for Model Types
  // type ModelType = {
  //   #Tabular;
  //   #ComputerVision;
  //   #NLP;
  //   #LLM;
  //   #VisionModel;
  //   #AudioModel;
  //   #Agents;
  // };

  // Complexity Enum to indicate model sophistication
  // type ModelComplexity = {
  //   #Beginner;
  //   #Intermediate;
  //   #Advanced;
  //   #Research;
  // };

  // Licensing Options
  // type LicenseType = {
  //   #OpenSource;
  //   #Commercial;
  //   #Academic;
  //   #ResearchOnly;
  // };

  // Performance Metrics
  // type PerformanceMetrics = {
  //   accuracy: ?Float;
  //   trainingTime: ?Text;
  //   modelSize: ?Text;
  // };

  // Main Model Structure
  // type AIModel = {
  //   id: Nat;
  //   name: Text;
  //   description: Text;
  //   modelType: ModelType;
  //   tags: [Text];
  //   link: Text;
  //   submitterName: Text;
  //   submitterLink: Text;
  //   complexity: ModelComplexity;
  //   licenseType: LicenseType;
  //   githubStars: ?Nat;
  //   paperLink: ?Text;
  //   publishedDate: Time.Time;
  //   frameworkUsed: ?Text;
  //   performanceMetrics: ?PerformanceMetrics;
  // };

  // Custom hash function for Nat
  // func natHash(n: Nat) : Hash.Hash {
  //   return Text.hash(Nat.toText(n));
  // };

  // Storage
  // let models = HashMap.HashMap<Nat, AIModel>(10, Nat.equal, natHash);
  // var nextId : Nat = 1;

  // Helper function to handle optional values
  // func mergeOptional<T>(newValue : ?T, existingValue : T) : T {
  //   switch (newValue) {
  //     case null { existingValue };
  //     case (?value) { value };
  //   }
  // };

  // Helper function to merge optional values with optional type
  // func mergeOptionalFields<T>(newValue : ?T, existingValue : ?T) : ?T {
  //   switch (newValue) {
  //     case null { existingValue };
  //     case (?value) { ?value };
  //   }
  // };

  // Create a new AI Model
  // public func createModel(
  //   name: Text, 
  //   description: Text, 
  //   modelType: ModelType,
  //   tags: [Text],
  //   link: Text,
  //   submitterName: Text,
  //   submitterLink: Text,
  //   complexity: ModelComplexity,
  //   licenseType: LicenseType,
  //   githubStars: ?Nat,
  //   paperLink: ?Text,
  //   frameworkUsed: ?Text,
  //   performanceMetrics: ?PerformanceMetrics
  // ) : async Result.Result<Nat, Text> {
  //   if (name == "" or link == "") {
  //     return #err("Name and link are required");
  //   };

  //   let newModel : AIModel = {
  //     id = nextId;
  //     name = name;
  //     description = description;
  //     modelType = modelType;
  //     tags = tags;
  //     link = link;
  //     submitterName = submitterName;
  //     submitterLink = submitterLink;
  //     complexity = complexity;
  //     licenseType = licenseType;
  //     githubStars = githubStars;
  //     paperLink = paperLink;
  //     publishedDate = Time.now();
  //     frameworkUsed = frameworkUsed;
  //     performanceMetrics = performanceMetrics;
  //   };

  //   models.put(nextId, newModel);
  //   let modelId = nextId;
  //   nextId += 1;

  //   #ok(modelId)
  // };

  // Read a specific model by ID
  // public query func getModel(id: Nat) : async ?AIModel {
  //   models.get(id)
  // };

  // Update an existing model
  // public func updateModel(
  //   id: Nat, 
  //   name: ?Text, 
  //   description: ?Text,
  //   modelType: ?ModelType,
  //   tags: ?[Text],
  //   link: ?Text,
  //   submitterName: ?Text,
  //   submitterLink: ?Text,
  //   complexity: ?ModelComplexity,
  //   licenseType: ?LicenseType,
  //   githubStars: ?Nat,
  //   paperLink: ?Text,
  //   frameworkUsed: ?Text,
  //   performanceMetrics: ?PerformanceMetrics
  // ) : async Result.Result<(), Text> {
  //   switch (models.get(id)) {
  //     case null { #err("Model not found") };
  //     case (?existingModel) {
  //       let updatedModel : AIModel = {
  //         id = existingModel.id;
  //         name = mergeOptional(name, existingModel.name);
  //         description = mergeOptional(description, existingModel.description);
  //         modelType = mergeOptional(modelType, existingModel.modelType);
  //         tags = mergeOptional(tags, existingModel.tags);
  //         link = mergeOptional(link, existingModel.link);
  //         submitterName = mergeOptional(submitterName, existingModel.submitterName);
  //         submitterLink = mergeOptional(submitterLink, existingModel.submitterLink);
  //         complexity = mergeOptional(complexity, existingModel.complexity);
  //         licenseType = mergeOptional(licenseType, existingModel.licenseType);
  //         githubStars = mergeOptionalFields(githubStars, existingModel.githubStars);
  //         paperLink = mergeOptionalFields(paperLink, existingModel.paperLink);
  //         publishedDate = existingModel.publishedDate;
  //         frameworkUsed = mergeOptionalFields(frameworkUsed, existingModel.frameworkUsed);
  //         performanceMetrics = mergeOptionalFields(performanceMetrics, existingModel.performanceMetrics);
  //       };

  //       models.put(id, updatedModel);
  //       #ok()
  //     };
  //   }
  // };

  // Delete a model
  // public func deleteModel(id: Nat) : async Result.Result<(), Text> {
  //   switch (models.get(id)) {
  //     case null { #err("Model not found") };
  //     case (?_) { 
  //       models.delete(id);
  //       #ok() 
  //     };
  //   }
  // };

  // List all models
  // public query func listModels() : async [(Nat, AIModel)] {
  //   let modelArray = Buffer.Buffer<(Nat, AIModel)>(models.size());
    
  //   for ((id, model) in models.entries()) {
  //     modelArray.add((id, model));
  //   };

  //   Buffer.toArray(modelArray)
  // };

  // Search models by type
  // public query func searchModelsByType(modelType: ModelType) : async [(Nat, AIModel)] {
  //   let filteredModels = Buffer.Buffer<(Nat, AIModel)>(0);
    
  //   for ((id, model) in models.entries()) {
  //     if (model.modelType == modelType) {
  //       filteredModels.add((id, model));
  //     }
  //   };

  //   Buffer.toArray(filteredModels)
  // };

  // Search models by tag
  // public query func searchModelsByTag(tag: Text) : async [(Nat, AIModel)] {
  //   let filteredModels = Buffer.Buffer<(Nat, AIModel)>(0);
    
  //   for ((id, model) in models.entries()) {
  //     if (Array.find(model.tags, func(t : Text) : Bool { t == tag }) != null) {
  //       filteredModels.add((id, model));
  //     }
  //   };

  //   Buffer.toArray(filteredModels)
  // };
// }