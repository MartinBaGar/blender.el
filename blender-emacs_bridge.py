import threading
import sys
import json
import os
import subprocess
import bpy

# === CONFIGURATION ===
ADDON_DIR = r"C:\Users\martb\Documents\Blender\my_addons"
ADDON_NAME = None
if "--" in sys.argv:
    idx = sys.argv.index("--")
    if len(sys.argv) > idx + 1:
        ADDON_NAME = sys.argv[idx + 1]
# ======================

# Add addon directory to sys.path if not already
if ADDON_DIR not in sys.path:
    sys.path.append(ADDON_DIR)

# Try to enable the addon
try:
    bpy.ops.preferences.addon_enable(module=ADDON_NAME)
    print(f"Addon '{ADDON_NAME}' enabled.", flush=True)
except Exception as e:
    print(f"Failed to enable addon '{ADDON_NAME}': {e}", flush=True)

def run_external_python(script_path, python_env):
    """Run a Python script in an external Python environment."""
    try:
        print(f"Running {script_path} in external Python: {python_env}", flush=True)
        
        result = subprocess.run([
            python_env, script_path
        ], capture_output=True, text=True, timeout=30)
        
        if result.stdout:
            print(f"External output: {result.stdout}", flush=True)
        if result.stderr:
            print(f"External stderr: {result.stderr}", flush=True)
        if result.returncode != 0:
            print(f"External script exited with code: {result.returncode}", flush=True)
            
    except subprocess.TimeoutExpired:
        print("External script timed out after 30 seconds", flush=True)
    except FileNotFoundError:
        print(f"Python executable not found: {python_env}", flush=True)
    except Exception as e:
        print(f"Error running external script: {e}", flush=True)

def evaluate_expression(expr):
    """Safely evaluate a Python expression and return the result."""
    try:
        # Create a safe namespace with common Blender modules
        namespace = {
            'bpy': bpy,
            'bmesh': None,  # Import on demand
            'mathutils': None,  # Import on demand
        }
        
        # Try to import common modules on demand
        try:
            import bmesh
            namespace['bmesh'] = bmesh
        except ImportError:
            pass
            
        try:
            from mathutils import Vector, Matrix, Euler
            namespace['mathutils'] = sys.modules['mathutils']
            namespace['Vector'] = Vector
            namespace['Matrix'] = Matrix
            namespace['Euler'] = Euler
        except ImportError:
            pass
        
        # Evaluate the expression
        result = eval(expr, {"__builtins__": {}}, namespace)
        print(f"Eval result: {result}", flush=True)
        return result
    except Exception as e:
        print(f"Eval error: {e}", flush=True)
        return None

def reload_addon_modules(addon_name):
    """Reload all modules related to an addon."""
    try:
        import importlib
        
        # Find and reload all modules related to the addon
        modules_to_reload = []
        for name in list(sys.modules.keys()):
            # Match addon modules (e.g., "test", "test.submodule", etc.)
            if name == addon_name or name.startswith(addon_name + "."):
                modules_to_reload.append(name)
        
        print(f"Found {len(modules_to_reload)} modules to reload: {modules_to_reload}", flush=True)
        
        # Reload in reverse order (children first, then parents)
        for module_name in reversed(modules_to_reload):
            if module_name in sys.modules:
                try:
                    importlib.reload(sys.modules[module_name])
                    print(f"Reloaded module: {module_name}", flush=True)
                except Exception as e:
                    print(f"Failed to reload {module_name}: {e}", flush=True)
        
        return True
    except Exception as e:
        print(f"Error in module reload: {e}", flush=True)
        return False

def emacs_bridge():
    """Main bridge loop handling commands from Emacs."""
    global ADDON_NAME
    print("Blender Emacs bridge ready", flush=True)
    
    while True:
        try:
            line = sys.stdin.readline()
            if not line:
                break
                
            cmd = json.loads(line.strip())
            command = cmd.get("cmd")
            
            if command == "run":
                path = cmd.get("path")
                external = cmd.get("external", False)
                python_env = cmd.get("python_env")
                
                if not os.path.exists(path):
                    print(f"Error: File not found - {path}", flush=True)
                    continue
                
                if external and python_env:
                    # Run in external Python
                    run_external_python(path, python_env)
                else:
                    # Run in Blender's Python
                    try:
                        with open(path, 'r', encoding='utf-8') as f:
                            code = f.read()
                        exec(compile(code, path, 'exec'), {'__name__': '__main__'})
                        print(f"Executed: {path}", flush=True)
                    except Exception as e:
                        print(f"Error in {path}: {e}", flush=True)
            
            elif command == "reload_addon":
                addon_name = cmd.get("addon", ADDON_NAME)
                try:
                    # Disable addon first
                    bpy.ops.preferences.addon_disable(module=addon_name)
                    print(f"Addon '{addon_name}' disabled.", flush=True)
                    
                    # Reload modules
                    if reload_addon_modules(addon_name):
                        # Re-enable addon
                        bpy.ops.preferences.addon_enable(module=addon_name)
                        print(f"Addon '{addon_name}' fully reloaded.", flush=True)
                    else:
                        print(f"Module reload failed for '{addon_name}'", flush=True)
                        
                except Exception as e:
                    print(f"Error reloading addon '{addon_name}': {e}", flush=True)
            
            elif command == "set_addon":
                new_name = cmd.get("name")
                if new_name:
                    ADDON_NAME = new_name
                    print(f"Active addon set to: {ADDON_NAME}", flush=True)
                   
            elif command == "eval":
                expression = cmd.get("expr")
                if expression:
                    evaluate_expression(expression)
                       
            else:
                print(f"Unknown command: {command}", flush=True)
                
        except json.JSONDecodeError as e:
            print(f"JSON decode error: {e}", flush=True)
        except Exception as e:
            print(f"Bridge error: {e}", flush=True)

# Launch the bridge in a separate thread
threading.Thread(target=emacs_bridge, daemon=True).start()
