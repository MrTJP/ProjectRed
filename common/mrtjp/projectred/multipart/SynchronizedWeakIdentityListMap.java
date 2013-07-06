package mrtjp.projectred.multipart;

import java.lang.ref.Reference;
import java.lang.ref.ReferenceQueue;
import java.lang.ref.WeakReference;
import java.util.AbstractMap;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;

public class SynchronizedWeakIdentityListMap<K, V> {

	private ReferenceQueue<K> refQueue = new ReferenceQueue<K>();

	private static class ListNode<K, V> {
		public WeakReference<K> key;
		public V value;
		public ListNode<K, V> next = null;
	}

	private ListNode<K, V> head = null;

	public synchronized void remove(K key) {
		removeCollectedKeys();

		if (head == null)
			return;
		if (head.key.get() == key) {
			head = head.next;
			return;
		}
		ListNode<K, V> cur = head;
		while (cur.next != null) {
			if (cur.next.key.get() == key) {
				cur.next = cur.next.next;
				return;
			}
			cur = cur.next;
		}
	}

	public synchronized V get(K key) {
		removeCollectedKeys();

		ListNode<K, V> cur = head;
		while (cur != null) {
			if (cur.key.get() == key)
				return cur.value;
			cur = cur.next;
		}
		return null;
	}

	public synchronized void put(K key, V val) {
		removeCollectedKeys();

		ListNode<K, V> cur = head;
		while (cur != null) {
			K curKey = cur.key.get();
			if (curKey != null && key == curKey) {
				cur.value = val;
				return;
			}
			cur = cur.next;
		}
		ListNode<K, V> n = new ListNode<K, V>();
		n.key = new WeakReference<K>(key, refQueue);
		n.value = val;
		n.next = head;
		head = n;
	}

	private void removeCollectedKeys() {
		Reference<? extends K> ref;
		while ((ref = refQueue.poll()) != null) {
			if (head == null)
				continue;

			if (head != null && head.key == ref) {
				head = head.next;
				continue;
			}
			ListNode<K, V> cur = head;
			while (cur.next != null) {
				if (cur.next.key == ref) {
					cur.next = cur.next.next;
					break;
				}
				cur = cur.next;
			}
		}
	}

	public synchronized Iterable<Map.Entry<K, V>> entries() {
		Collection<Map.Entry<K, V>> rv = new ArrayList<Map.Entry<K, V>>();

		ListNode<K, V> cur = head;
		while (cur != null) {
			K curKey = cur.key.get();
			if (curKey != null) {
				rv.add(new AbstractMap.SimpleEntry<K, V>(curKey, cur.value));
			}
			cur = cur.next;
		}

		return rv;
	}

}
