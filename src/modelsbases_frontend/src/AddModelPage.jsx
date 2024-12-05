import React, { useState } from 'react';
import { useNavigate } from 'react-router-dom';
import { modelsbases_backend } from 'declarations/modelsbases_backend';

function AddModelPage() {
  const navigate = useNavigate();
  const [isLoading, setIsLoading] = useState(false);
  const [error, setError] = useState(null);
  const [formData, setFormData] = useState({
    name: '',
    description: '',
    modelType: '',
    githubLink: '',
    articleLink: '',
    submitterLinkedIn: ''
  });

  // Handle input changes
  const handleInputChange = (e) => {
    const { name, value } = e.target;
    setFormData(prev => ({
      ...prev,
      [name]: value
    }));
  };

  // Form submission handler
  const handleSubmit = async (e) => {
    e.preventDefault();
    setIsLoading(true);
    setError(null);

    try {
      await modelsbases_backend.createModel(
        formData.name,
        formData.description,
        { [formData.modelType]: null },
        formData.githubLink,
        formData.articleLink ? [formData.articleLink] : [],
        formData.submitterLinkedIn
      );

      // Redirect to model list on successful submission
      navigate('/');
    } catch (err) {
      console.error('Error creating model:', err);
      setError('Failed to create model. Please check your inputs and try again.');
      setIsLoading(false);
    }
  };

  return (
    <div className="min-h-screen bg-gray-100 flex items-center justify-center p-6">
      <div className="w-full max-w-md bg-white rounded-lg shadow-md p-8">
        <h2 className="text-2xl font-bold text-center text-gray-800 mb-6">
          Add New AI Model
        </h2>

        {error && (
          <div className="bg-red-100 border border-red-400 text-red-700 px-4 py-3 rounded relative mb-4" role="alert">
            {error}
          </div>
        )}

        <form onSubmit={handleSubmit} className="space-y-4">
          <div>
            <label htmlFor="name" className="block text-sm font-medium text-gray-700">
              Model Name
            </label>
            <input
              type="text"
              id="name"
              name="name"
              value={formData.name}
              onChange={handleInputChange}
              required
              placeholder="Enter model name"
              className="mt-1 block w-full px-3 py-2 border border-gray-300 rounded-md shadow-sm focus:outline-none focus:ring-blue-500 focus:border-blue-500"
            />
          </div>

          <div>
            <label htmlFor="description" className="block text-sm font-medium text-gray-700">
              Description
            </label>
            <textarea
              id="description"
              name="description"
              value={formData.description}
              onChange={handleInputChange}
              required
              placeholder="Describe the AI model"
              rows="4"
              className="mt-1 block w-full px-3 py-2 border border-gray-300 rounded-md shadow-sm focus:outline-none focus:ring-blue-500 focus:border-blue-500"
            />
          </div>

          <div>
            <label htmlFor="modelType" className="block text-sm font-medium text-gray-700">
              Model Type
            </label>
            <select
              id="modelType"
              name="modelType"
              value={formData.modelType}
              onChange={handleInputChange}
              required
              className="mt-1 block w-full px-3 py-2 border border-gray-300 rounded-md shadow-sm focus:outline-none focus:ring-blue-500 focus:border-blue-500"
            >
              <option value="">Select Model Type</option>
              <option value="NLP">Natural Language Processing (NLP)</option>
              <option value="TabularData">Tabular Data</option>
              <option value="ComputerVision">Computer Vision</option>
              <option value="SpeechRecognition">Speech Recognition</option>
              <option value="Other">Other</option>
            </select>
          </div>

          <div>
            <label htmlFor="githubLink" className="block text-sm font-medium text-gray-700">
              GitHub Repository
            </label>
            <input
              type="url"
              id="githubLink"
              name="githubLink"
              value={formData.githubLink}
              onChange={handleInputChange}
              required
              placeholder="GitHub repository URL"
              className="mt-1 block w-full px-3 py-2 border border-gray-300 rounded-md shadow-sm focus:outline-none focus:ring-blue-500 focus:border-blue-500"
            />
          </div>

          <div>
            <label htmlFor="articleLink" className="block text-sm font-medium text-gray-700">
              Article Link (Optional)
            </label>
            <input
              type="url"
              id="articleLink"
              name="articleLink"
              value={formData.articleLink}
              onChange={handleInputChange}
              placeholder="Related article URL"
              className="mt-1 block w-full px-3 py-2 border border-gray-300 rounded-md shadow-sm focus:outline-none focus:ring-blue-500 focus:border-blue-500"
            />
          </div>

          <div>
            <label htmlFor="submitterLinkedIn" className="block text-sm font-medium text-gray-700">
              Submitter's LinkedIn
            </label>
            <input
              type="url"
              id="submitterLinkedIn"
              name="submitterLinkedIn"
              value={formData.submitterLinkedIn}
              onChange={handleInputChange}
              required
              placeholder="LinkedIn profile URL"
              className="mt-1 block w-full px-3 py-2 border border-gray-300 rounded-md shadow-sm focus:outline-none focus:ring-blue-500 focus:border-blue-500"
            />
          </div>

          <div className="flex space-x-4">
            <button
              type="submit"
              disabled={isLoading}
              className="w-full bg-blue-600 text-white py-2 px-4 rounded-md hover:bg-blue-700 focus:outline-none focus:ring-2 focus:ring-blue-500 focus:ring-offset-2 disabled:opacity-50"
            >
              {isLoading ? 'Submitting...' : 'Submit Model'}
            </button>
            <button
              type="button"
              onClick={() => navigate('/')}
              className="w-full bg-gray-200 text-gray-700 py-2 px-4 rounded-md hover:bg-gray-300 focus:outline-none focus:ring-2 focus:ring-gray-500 focus:ring-offset-2"
            >
              Cancel
            </button>
          </div>
        </form>
      </div>
    </div>
  );
}

export default AddModelPage;