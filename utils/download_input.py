import argparse
import os
import sys
import requests

def download_input(year, day, session_cookie, output_path):
    """Downloads the input for a specific year and day from Advent of Code."""
    url = f"https://adventofcode.com/{year}/day/{day}/input"
    cookies = {"session": session_cookie}
    # AoC requests a User-Agent with contact info
    headers = {"User-Agent": "github.com/andreabarghetti/AdventOfCode Input Downloader"}

    try:
        response = requests.get(url, cookies=cookies, headers=headers)
        response.raise_for_status()
    except requests.exceptions.RequestException as e:
        print(f"Error downloading input: {e}")
        sys.exit(1)

    with open(output_path, "w") as f:
        f.write(response.text)
    
    print(f"Successfully downloaded input for {year} Day {day} to {output_path}")

def main():
    parser = argparse.ArgumentParser(description="Download Advent of Code input.")
    parser.add_argument("year", type=int, help="The year of the puzzle.")
    parser.add_argument("day", type=int, help="The day of the puzzle.")
    parser.add_argument("--output", "-o", help="Output file path.", default="input.txt")
    parser.add_argument("--session", "-s", help="Session cookie string.")
    
    args = parser.parse_args()

    session_cookie = args.session
    
    # Check for session cookie in file or environment variable if not provided
    if not session_cookie:
        session_file = os.path.join(os.path.dirname(__file__), ".session")
        if os.path.exists(session_file):
            with open(session_file, "r") as f:
                session_cookie = f.read().strip()
        elif "AOC_SESSION" in os.environ:
            session_cookie = os.environ["AOC_SESSION"]

    if not session_cookie:
        print("Error: Session cookie not found. Please provide it via --session, a .session file, or AOC_SESSION environment variable.")
        sys.exit(1)

    download_input(args.year, args.day, session_cookie, args.output)

if __name__ == "__main__":
    main()