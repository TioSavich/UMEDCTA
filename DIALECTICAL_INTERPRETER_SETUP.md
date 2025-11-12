# Dialectical Interpreter Setup Guide

## You're Almost There!

The app includes both a React frontend and a Node.js backend to securely handle API calls.

## Final Setup Step

Create a file named `.env` in the root directory with your API key:

```bash
VITE_ANTHROPIC_API_KEY=sk-ant-api-YOUR-KEY-HERE
```

Replace `sk-ant-api-YOUR-KEY-HERE` with your actual Anthropic API key.

**Important**: The `.env` file is already in `.gitignore`, so it won't be committed to GitHub.

## Running the Application

1. **Start both servers** (backend + frontend):
   ```bash
   npm run dev
   ```

   This starts:
   - Backend server on `http://localhost:3001` (API proxy)
   - Frontend on `http://localhost:3000` (React app)

2. **Open your browser** to `http://localhost:3000`

3. **Start interpreting!** Paste philosophical text and click "Interpret Text"

## Architecture

The app uses a **client-server architecture** to avoid CORS issues:
- **Frontend** (Vite + React): User interface
- **Backend** (Express): Proxies requests to Anthropic API with your key
- This keeps your API key secure and avoids browser CORS restrictions

## Project Structure

```
UMEDCTA/
├── .env                          # Your API key (create this!)
├── .env.example                  # Template for API key
├── .gitignore                    # Protects your API key
├── server.js                     # Backend API proxy server
├── index.html                    # Main HTML file
├── vite.config.js               # Vite configuration
├── package.json                  # Dependencies and scripts
└── src/
    ├── main.jsx                  # React entry point
    └── DialecticalInterpreter.jsx # Main component
```

## Features

- **PML Formalization**: Converts text into Polarized Modal Logic
- **Proof Steps**: Shows logical derivations
- **Meta-Critique**: Compares against scholarly readings
- **Self-Evolution**: Proposes and integrates new axioms
- **Conversation Mode**: Ask follow-up questions
- **Re-reading Support**: Tracks iteration depth and formalized concepts

## Cost Estimate

Each interpretation uses the Claude Sonnet 4 API:
- Typical cost: $0.10-0.30 per interpretation
- Pricing: $3/million input tokens, $15/million output tokens

## Troubleshooting

**API key error**: Make sure your `.env` file exists and has the correct format
**Module errors**: Run `npm install` again
**Port in use**: Change the port in `vite.config.js`

## Next Steps

See [Prolog/dialectical-interpreter-README.md](Prolog/dialectical-interpreter-README.md) for information about the temporal phenomenology approach and how to use the interpreter effectively.
