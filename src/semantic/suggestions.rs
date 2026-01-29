//! Symbol name suggestions for error messages.
//!
//! This module provides fuzzy matching to suggest similar symbol names when
//! a user references an undefined variable, label, or procedure. This helps
//! catch typos and provides better error messages.

/// Computes the Levenshtein distance between two strings.
///
/// The Levenshtein distance is the minimum number of single-character edits
/// (insertions, deletions, or substitutions) required to transform one string
/// into another. Lower values indicate more similar strings.
///
/// # Example
///
/// ```ignore
/// assert_eq!(levenshtein_distance("foo", "bar"), 3);
/// assert_eq!(levenshtein_distance("foo", "fo"), 1);
/// assert_eq!(levenshtein_distance("foo", "foo"), 0);
/// ```
fn levenshtein_distance(s1: &str, s2: &str) -> usize {
    let s1_chars: Vec<char> = s1.chars().collect();
    let s2_chars: Vec<char> = s2.chars().collect();
    let s1_len = s1_chars.len();
    let s2_len = s2_chars.len();

    // Handle empty strings
    if s1_len == 0 {
        return s2_len;
    }
    if s2_len == 0 {
        return s1_len;
    }

    // Create a matrix to store distances
    let mut matrix = vec![vec![0; s2_len + 1]; s1_len + 1];

    // Initialize first row and column
    #[allow(clippy::needless_range_loop)]
    for i in 0..=s1_len {
        matrix[i][0] = i;
    }
    #[allow(clippy::needless_range_loop)]
    for j in 0..=s2_len {
        matrix[0][j] = j;
    }

    // Fill the matrix
    for i in 1..=s1_len {
        for j in 1..=s2_len {
            let cost = if s1_chars[i - 1] == s2_chars[j - 1] {
                0
            } else {
                1
            };
            matrix[i][j] = (matrix[i - 1][j] + 1)
                .min(matrix[i][j - 1] + 1)
                .min(matrix[i - 1][j - 1] + cost);
        }
    }

    matrix[s1_len][s2_len]
}

/// Computes a similarity score between two strings (0.0 to 1.0).
///
/// Higher scores indicate more similar strings. The score is based on
/// Levenshtein distance, normalized by the maximum string length.
///
/// # Example
///
/// ```ignore
/// assert!(similarity_score("foo", "foo") > 0.99);
/// assert!(similarity_score("foo", "bar") < 0.5);
/// assert!(similarity_score("count", "counter") > 0.7);
/// ```
fn similarity_score(s1: &str, s2: &str) -> f64 {
    let distance = levenshtein_distance(s1, s2);
    let max_len = s1.len().max(s2.len());
    if max_len == 0 {
        return 1.0;
    }
    1.0 - (distance as f64 / max_len as f64)
}

/// Finds the best matching symbol name from a list of candidates.
///
/// Returns the most similar name if it meets the similarity threshold,
/// or None if no candidate is similar enough.
///
/// # Arguments
///
/// * `target` - The name that was not found (e.g., "countr" for typo of "counter")
/// * `candidates` - List of available symbol names to search
/// * `threshold` - Minimum similarity score (0.0 to 1.0) to consider a match
///
/// # Example
///
/// ```ignore
/// let candidates = vec!["counter", "count", "total"];
/// assert_eq!(find_best_match("countr", &candidates, 0.6), Some("counter"));
/// assert_eq!(find_best_match("xyz", &candidates, 0.6), None);
/// ```
pub fn find_best_match(target: &str, candidates: &[String], threshold: f64) -> Option<String> {
    let target_lower = target.to_lowercase();
    let mut best_match: Option<(String, f64)> = None;

    for candidate in candidates {
        let candidate_lower = candidate.to_lowercase();
        let score = similarity_score(&target_lower, &candidate_lower);

        // Update best match if this is better
        if score >= threshold {
            match &best_match {
                None => best_match = Some((candidate.clone(), score)),
                Some((_, best_score)) if score > *best_score => {
                    best_match = Some((candidate.clone(), score));
                }
                _ => {}
            }
        }
    }

    best_match.map(|(name, _)| name)
}

/// Finds multiple similar symbol names, sorted by similarity.
///
/// Returns up to `max_results` candidates that meet the similarity threshold,
/// sorted from most similar to least similar.
///
/// # Arguments
///
/// * `target` - The name that was not found
/// * `candidates` - List of available symbol names to search
/// * `threshold` - Minimum similarity score to consider
/// * `max_results` - Maximum number of suggestions to return
///
/// # Example
///
/// ```ignore
/// let candidates = vec!["counter", "count", "total", "amount"];
/// let suggestions = find_similar_names("countr", &candidates, 0.5, 3);
/// assert_eq!(suggestions, vec!["counter", "count"]);
/// ```
pub fn find_similar_names(
    target: &str,
    candidates: &[String],
    threshold: f64,
    max_results: usize,
) -> Vec<String> {
    let target_lower = target.to_lowercase();
    let mut matches: Vec<(String, f64)> = Vec::new();

    for candidate in candidates {
        let candidate_lower = candidate.to_lowercase();
        let score = similarity_score(&target_lower, &candidate_lower);

        if score >= threshold {
            matches.push((candidate.clone(), score));
        }
    }

    // Sort by score (descending) and take top results
    matches.sort_by(|a, b| b.1.partial_cmp(&a.1).unwrap_or(std::cmp::Ordering::Equal));
    matches
        .into_iter()
        .take(max_results)
        .map(|(name, _)| name)
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_levenshtein_distance() {
        assert_eq!(levenshtein_distance("", ""), 0);
        assert_eq!(levenshtein_distance("foo", ""), 3);
        assert_eq!(levenshtein_distance("", "bar"), 3);
        assert_eq!(levenshtein_distance("foo", "foo"), 0);
        assert_eq!(levenshtein_distance("foo", "bar"), 3);
        assert_eq!(levenshtein_distance("kitten", "sitting"), 3);
        assert_eq!(levenshtein_distance("count", "counter"), 3);
    }

    #[test]
    fn test_similarity_score() {
        assert!((similarity_score("foo", "foo") - 1.0).abs() < 0.01);
        assert!(similarity_score("foo", "bar") < 0.5);
        assert!(similarity_score("count", "counter") > 0.5);
    }

    #[test]
    fn test_find_best_match() {
        let candidates = vec![
            "counter".to_string(),
            "count".to_string(),
            "total".to_string(),
        ];

        // Exact match
        assert_eq!(
            find_best_match("counter", &candidates, 0.6),
            Some("counter".to_string())
        );

        // Close typo
        assert_eq!(
            find_best_match("countr", &candidates, 0.6),
            Some("counter".to_string())
        );

        // Not similar enough
        assert_eq!(find_best_match("xyz", &candidates, 0.6), None);

        // Case insensitive
        assert_eq!(
            find_best_match("COUNTER", &candidates, 0.6),
            Some("counter".to_string())
        );
    }

    #[test]
    fn test_find_similar_names() {
        let candidates = vec![
            "counter".to_string(),
            "count".to_string(),
            "total".to_string(),
            "amount".to_string(),
        ];

        let suggestions = find_similar_names("countr", &candidates, 0.5, 3);
        assert!(suggestions.contains(&"counter".to_string()));
        assert!(suggestions.contains(&"count".to_string()));
        assert!(suggestions.len() <= 3);
    }
}
