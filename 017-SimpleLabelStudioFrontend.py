#!/usr/bin/env python3
import os
import subprocess
from pathlib import Path
import shutil

def start_label_studio():
    # Set up virtual environment
    venv_path = Path.cwd() / "virtuuvapp"
    
    # Check if virtual environment exists
    if not venv_path.exists():
        print(f"Error: Virtual environment not found at {venv_path}")
        return False
    
    # Check for label-studio executable
    label_studio_exe = venv_path / "Scripts" / "label-studio.exe"
    if not label_studio_exe.exists():
        print(f"Error: label-studio not found at {label_studio_exe}")
        print("Try installing it with: uv add label-studio")
        return False
    
    env = os.environ.copy()
    env["VIRTUAL_ENV"] = str(venv_path)
    env["PATH"] = str(venv_path / "Scripts") + os.pathsep + env["PATH"]
    
    print(f"Using virtual environment: {venv_path}")
    print(f"Label Studio executable: {label_studio_exe}")
    print("Press Ctrl+C to stop")
    
    try:
        subprocess.run(
            [str(label_studio_exe), 'start'],
            cwd=Path.cwd(),
            env=env,
            check=True
        )
    except KeyboardInterrupt:
        print("\nStopping Label Studio...")
    except FileNotFoundError:
        print("Error: label-studio executable not found")
        return False
    except subprocess.CalledProcessError as e:
        print(f"Error starting Label Studio: {e}")
        return False
    
    return True

if __name__ == "__main__":
    start_label_studio()
