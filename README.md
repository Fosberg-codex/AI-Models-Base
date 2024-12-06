# AI-Models-Base

AI-Models-Base is a full-stack AI models directory site built on the Internet Computer (ICP) and powered by Motoko. It enables users to discover, submit, and manage AI models seamlessly, leveraging the decentralized and scalable architecture of ICP.

## Key Features

- **AI Model Directory**: Users can browse a comprehensive library of AI models.
- **Model Submission**: Submit new AI models with metadata and details for sharing.
- **Decentralized Backend**: Powered by Motoko on ICP for scalable, secure, and decentralized operations.
- **Dynamic Frontend**: React-based interface for a seamless user experience.
- **ICP Integration**: Harness the power of the Internet Computer for distributed hosting and management.

---

## Getting Started

### Prerequisites

To run this project, ensure you have the following tools installed:

- **DFX SDK**: Internet Computer SDK for development and deployment. [Install DFX](https://internetcomputer.org/docs/current/developer-docs/setup/install)
- **Node.js**: JavaScript runtime for frontend development.
- **npm**: Package manager for JavaScript.

### Project Structure

The project is organized as follows:

```
AI-Models-Base/
|---
├── src/
│   ├── declarations/     # Auto-generated type declarations
│   ├── modelsbases_backend/    # Backend canister code
│   │   ├── main.mo            # Core backend logic
│   │   └── types.mo           # Type definitions
│   └── modelsbases_frontend/   # Frontend application
│       ├── src/               # React components and pages
│       ├── assets/            # Static assets
│       └── package.json       # Frontend dependencies
├── dfx.json           # Project configuration
├── package.json       # Root package.json
└── README.md          # Project documentation
```

---

## Running the Project Locally

### Step 1: Start the Replica
Start the local ICP replica to emulate the Internet Computer environment:

```bash
dfx start --clean --background
```

### Step 2: Deploy the Canisters
Deploy both backend and frontend canisters to the local replica:

```bash
dfx deploy
```

Once deployment is complete, your application will be available at:

```plaintext
http://localhost:4943?canisterId=<frontend_canister_id>
```

### Step 3: Start the Frontend
For frontend development, start the React development server:

```bash
npm start
```

This will start a server at `http://localhost:8080`, proxying API requests to the replica.

---

## Interacting with the Backend

The backend canister, written in Motoko, provides core functionality for managing AI models. Use the following commands to interact with the backend:

### Viewing the Backend Candid Interface

You can inspect the backend's Candid interface using:

```bash
dfx canister call <backend_canister_id> __get_candid_interface_tmp_hack
```

### Adding a Model via Candid

Use the Candid interface to add a new AI model:

```bash
dfx canister call <backend_canister_id> addModel '("Model Name", "Description", "URL", "Tags")'
```

---

## Deployment to the Internet Computer

Deploying to the ICP mainnet follows similar steps:

1. Update the `dfx.json` file to set the network to `ic`:

    ```json
    {
      "networks": {
        "ic": {
          "providers": ["https://ic0.app"],
          "type": "persistent"
        }
      }
    }
    ```

2. Deploy the canisters to the mainnet:

    ```bash
    dfx deploy --network ic
    ```

3. Access your app at the generated ICP URL.

---

## Technical Details

### Backend: Motoko
- **Language**: Motoko
- **Key Components**:
  - Canister storage for AI model metadata.
  - CRUD operations for managing models.

### Frontend: React
- **Framework**: React
- **Routing**: React Router
- **State Management**: Context API/State Hooks

### ICP Advantages
- **Scalability**: Run seamlessly on a global decentralized network.
- **Security**: Leverages cryptographic guarantees.
- **Cost Efficiency**: Low hosting costs.

---

## Useful Commands

```bash
# Start the ICP replica
$ dfx start --clean --background

# Deploy canisters
$ dfx deploy

# View canister details
$ dfx canister info <canister_name>

# Call canister methods
$ dfx canister call <canister_name> <method_name>
```

---

## Resources

- [Internet Computer Documentation](https://internetcomputer.org/docs/current/developer-docs/)
- [Motoko Programming Guide](https://internetcomputer.org/docs/current/motoko/main/motoko)
- [React Documentation](https://react.dev)

---

## Contributing

Contributions are welcome! Feel free to submit issues or pull requests.

---

## License

This project is licensed under the MIT License. See `LICENSE` for details.

