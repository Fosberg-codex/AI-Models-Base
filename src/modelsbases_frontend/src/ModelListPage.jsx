import React, { useState, useEffect } from 'react';
import { useNavigate } from 'react-router-dom';
import { modelsbases_backend } from 'declarations/modelsbases_backend';

function ModelListPage() {
  const navigate = useNavigate();
  const [models, setModels] = useState([]);
  const [filterType, setFilterType] = useState(null);
  const [isLoading, setIsLoading] = useState(true);
  const [error, setError] = useState(null);

  // Color mapping for model types
  const getModelTypeColor = (type) => {
    const typeStr = Object.keys(type)[0];
    switch(typeStr) {
      case 'NLP': return 'bg-blue-50 text-blue-800 border-orange-200';
      case 'TabularData': return 'bg-green-50 text-green-800 border-orange-200';
      case 'ComputerVision': return 'bg-purple-50 text-purple-800 border-orange-200';
      case 'SpeechRecognition': return 'bg-yellow-50 text-yellow-800 border-orange-200';
      case 'Other': return 'bg-gray-50 text-gray-800 border-orange-200';
      default: return 'bg-gray-50 text-gray-800 border-orange-200';
    }
  };

  // Fetch models
  useEffect(() => {
    const fetchModels = async () => {
      setIsLoading(true);
      setError(null);
      try {
        const fetchedModels = filterType
          ? await modelsbases_backend.getModelsByType({ [filterType]: null })
          : await modelsbases_backend.getAllModels();
        setModels(fetchedModels);
      } catch (err) {
        console.error('Error fetching models:', err);
        setError('Failed to load models. Please try again.');
      } finally {
        setIsLoading(false);
      }
    };

    fetchModels();
  }, [filterType]);

  // Delete model handler
  const handleDeleteModel = async (id) => {
    if (!window.confirm('Are you sure you want to delete this model?')) return;

    try {
      setIsLoading(true);
      await modelsbases_backend.deleteModel(id);
      
      // Refresh models after deletion
      const updatedModels = filterType
        ? await modelsbases_backend.getModelsByType({ [filterType]: null })
        : await modelsbases_backend.getAllModels();
      
      setModels(updatedModels);
    } catch (err) {
      console.error('Error deleting model:', err);
      setError('Failed to delete model. Please try again.');
    } finally {
      setIsLoading(false);
    }
  };

  return (
    <div className="min-h-screen bg-gray-100 p-6">
      <div className="container mx-auto w-3/4">
        <div className="flex justify-between items-center mb-6">
          <h1 className="text-3xl font-bold text-gray-800">AI <span className='py-1 px-2 bg-orange-700 text-white'>ModelsBase</span></h1>
          <button 
            onClick={() => navigate('/add-model')}
            className="bg-blue-600 text-white px-4 py-2 rounded-md hover:bg-blue-700 transition-colors"
          >
            Add New Model
          </button>
        </div>

        {/* Filter Dropdown */}
        <div className="mb-6">
          <select 
            value={filterType || ''}
            onChange={(e) => setFilterType(e.target.value || null)}
            className="w-full p-2 border rounded-md"
          >
            <option value="">All Model Types</option>
            <option value="NLP">Natural Language Processing (NLP)</option>
            <option value="TabularData">Tabular Data</option>
            <option value="ComputerVision">Computer Vision</option>
            <option value="SpeechRecognition">Speech Recognition</option>
            <option value="Other">Other</option>
          </select>
        </div>

        {/* Loading and Error States */}
        {isLoading && (
          <div className="text-center text-gray-500 py-6">Loading models...</div>
        )}

        {error && (
          <div className="bg-red-100 border border-red-400 text-red-700 px-4 py-3 rounded relative" role="alert">
            {error}
          </div>
        )}

        {/* Models Grid */}
        {!isLoading && !error && (
          <div className="grid md:grid-cols-2 lg:grid-cols-3 gap-4">
            {models.length === 0 ? (
              <div className="col-span-full text-center text-gray-500 py-6">
                No models found. Add a new model to get started!
              </div>
            ) : (
              models.map(model => (
                <div 
                  key={model.id} 
                  className={`border-2 rounded-lg p-3 shadow-sm ${getModelTypeColor(model.modelType)}`}
                >
                  <h2 className="text-md font-semibold mb-1">{model.name}</h2>
                  <p className="text-xs text-gray-600 mb-2">{model.description}</p>
                  
                  <div className="text-xs text-gray-500 mb-2">
                    Type: {Object.keys(model.modelType)[0]}
                  </div>
                  
                  <div className="flex flex-col space-y-1">
                    <a 
                      href={model.githubLink} 
                      target="_blank" 
                      rel="noopener noreferrer"
                      className="text-blue-600 hover:underline text-xs"
                    >
                      GitHub Repository
                    </a>
                    
                    {model.articleLink[0] && (
                      <a 
                        href={model.articleLink[0]} 
                        target="_blank" 
                        rel="noopener noreferrer"
                        className="text-green-600 hover:underline text-xs"
                      >
                        Related Article
                      </a>
                    )}
                  </div>
                  
                  <div className="mt-3 flex justify-between items-center">
                    <a 
                      href={model.submitterLinkedIn} 
                      target="_blank" 
                      rel="noopener noreferrer"
                      className="text-xs text-gray-600 hover:underline"
                    >
                      Submitter's Profile
                    </a>
                    
                    <button 
                      onClick={() => handleDeleteModel(model.id)}
                      disabled={isLoading}
                      className="text-red-500 hover:text-red-700 text-xs disabled:opacity-50"
                    >
                      Delete
                    </button>
                  </div>
                </div>
              ))
            )}
          </div>
        )}
      </div>
    </div>
  );
}

export default ModelListPage;