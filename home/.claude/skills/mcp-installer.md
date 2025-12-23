# MCP Server Installation Skill

## Overview
This skill helps install and configure ONLY APPROVED Model Context Protocol (MCP) servers for Claude Desktop.

## CRITICAL REQUIREMENTS

**BEFORE installing ANY MCP server, you MUST:**

1. **Search for and retrieve the AI Tool Guide from Confluence**
   - Use the mcp__atlassian__search tool to find "AI Tool Guide"
   - Use the mcp__atlassian__getConfluencePage tool to retrieve the page content
   - Find the section listing approved MCP servers

2. **Verify the requested MCP is approved**
   - Check if the MCP server appears in the AI Tool Guide's approved list
   - If NOT in the approved list: **REFUSE to install** and inform the user it's not approved
   - Only proceed if the MCP is explicitly listed as approved

## Installation Process

ONLY proceed with these steps if the MCP is approved per the AI Tool Guide:

1. **Locate the Claude Desktop config file**
   - macOS: `~/Library/Application Support/Claude/claude_desktop_config.json`
   - Windows: `%APPDATA%\Claude\claude_desktop_config.json`
   - Linux: `~/.config/Claude/claude_desktop_config.json`

2. **Read the existing config** using the Read tool to preserve existing servers

3. **Follow the EXACT installation instructions from the AI Tool Guide**
   - Use the command format specified in the guide
   - Use the package names specified in the guide
   - Configure any required environment variables as documented

4. **Update the config file** with the new server entry using the format from the AI Tool Guide

5. **Verify the configuration** is valid JSON

6. **Inform the user** they need to restart Claude Desktop for changes to take effect

## Important Notes

- **NEVER install an MCP that is not in the AI Tool Guide's approved list**
- Always use the installation instructions from the AI Tool Guide, not generic instructions
- Always preserve existing MCP server configurations
- Validate JSON before writing
- Never hardcode sensitive credentials in the config
- If the AI Tool Guide cannot be found or accessed, REFUSE the installation request
