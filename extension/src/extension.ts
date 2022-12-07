import * as vscode from "vscode";
import * as http from "http";
import axios from "axios";

import { execSync } from "child_process";
import { writeFileSync } from "fs";
import { join } from "path";

// Constants:
const PORT = 9999; // I am waiting for the exhale

// Variables:
let queue: Array<string> = [];

const Server = http.createServer((req, res) => {
	if (req.method == "GET") {
		if (req.url == "/api/receive") {
			if (queue.length > 0) {
				res.writeHead(200);
				res.end(queue.shift());
			} else {
				res.writeHead(204);
				res.end("");
			}
		}
	}
})

Server.listen(PORT, () => {
	console.log(`VSCode to Roblox Websocket is running on port ${PORT}`);
})

export function activate(context: vscode.ExtensionContext) {
	context.subscriptions.push(vscode.commands.registerCommand("vsc2rbx.execute", () => {
		const editor = vscode.window.activeTextEditor;

		if (editor && editor.document.languageId == "lua") {
			const document = editor.document;
			const text = document.getText();
			queue.push(text);
		}
	}));

	context.subscriptions.push(vscode.commands.registerCommand("vsc2rbx.installPlugin", async () => {
		vscode.window.showInformationMessage("Installing plugin...");
		
		const current_dir = __dirname;

		execSync("cd %LOCALAPPDATA%\\Roblox Studio\\Plugins");

		const response = await axios.get("https://pastebin.com/raw/2ug0CwaL");

		writeFileSync(join(__dirname, "VSC2RBX.rbxmx"), response.data);
		execSync(`cd ${current_dir}`);

		vscode.window.showInformationMessage("Plugin installed!");
	}));

	context.subscriptions.push(vscode.commands.registerCommand("vsc2rbx.activate", () => {
		vscode.window.showInformationMessage("Extension is activated");
	}));

	let item = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Left);
	item.text = "$(debug-start) Execute Script";
	item.tooltip = "Execute the currently opened script in roblox studio.";
	item.command = "vsc2rbx.execute";
	item.show();
}

export function deactivate() {}