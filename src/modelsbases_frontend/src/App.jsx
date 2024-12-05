import React from 'react';
import { BrowserRouter as Router, Routes, Route } from 'react-router-dom';
import ModelListPage from './ModelListPage';
import AddModelPage from './AddModelPage';
import './App.css'; 
import './index.scss'; 

function App() {
  return (
    <Router>
      <Routes>
        <Route path="/" element={<ModelListPage />} />
        <Route path="/add-model" element={<AddModelPage />} />
      </Routes>
    </Router>
  );
}

export default App;
