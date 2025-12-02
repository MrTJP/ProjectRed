import fire
import requests
import logging
import sys

# Logging
fmt_str = '[%(asctime)s] %(levelname)s %(lineno)d: %(message)s'
logging.basicConfig(level=logging.INFO, format=fmt_str)
log = logging.getLogger(__name__)


def mark_version(mod_id: str, mc_version: str, mod_version: str, homepage: str, api_key: str, type: str):
    """
    Marks a mod version as either 'latest' or 'recommended' in the version checker API.

    Parameters:
    -----------
    mod_id: The mod identifier (e.g., 'project-red-core')
    mc_version: The Minecraft version (e.g., '1.21.1')
    mod_version: The mod version to mark (e.g., '5.0.0-beta+1')
    homepage: The homepage URL for the mod
    api_key: API key for authentication
    type: Either 'latest' or 'recommended'
    """

    # Validate type parameter
    if type not in ['latest', 'recommended']:
        log.error(f"Invalid type: {type}. Must be 'latest' or 'recommended'")
        sys.exit(1)

    # Construct the API endpoint
    endpoint = f"https://version-check.covers1624.net/api/v2/mark_{type}"

    # Build request payload
    payload = {
        "modId": mod_id,
        "mcVersion": mc_version,
        "modVersion": mod_version,
        "homepage": homepage
    }

    # Build headers
    headers = {
        "API-Key": api_key,
        "Content-Type": "application/json"
    }

    log.info(f"Marking {mod_id} v{mod_version} (MC {mc_version}) as {type}...")
    log.debug(f"POST {endpoint}")
    log.debug(f"Payload: {payload}")

    try:
        response = requests.post(endpoint, json=payload, headers=headers, timeout=30)
        response.raise_for_status()
        log.info(f"Successfully marked {mod_id} as {type}")
        return True

    except requests.exceptions.HTTPError as e:
        log.error(f"HTTP error occurred: {e}")
        log.error(f"Response status: {response.status_code}")
        log.error(f"Response body: {response.text}")
        sys.exit(1)

    except requests.exceptions.ConnectionError as e:
        log.error(f"Connection error occurred: {e}")
        sys.exit(1)

    except requests.exceptions.Timeout as e:
        log.error(f"Request timed out: {e}")
        sys.exit(1)

    except requests.exceptions.RequestException as e:
        log.error(f"An error occurred: {e}")
        sys.exit(1)


if __name__ == '__main__':
    fire.Fire(mark_version)